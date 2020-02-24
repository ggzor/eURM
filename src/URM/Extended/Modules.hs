{-# LANGUAGE  NoImplicitPrelude #-}
module URM.Extended.Modules where

import URM.Extended.Compiler
import URM.Extended.Core as U
import URM.Extended.Parser hiding (declaration, name)

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Input.Utils
import Polysemy.State

import Control.Lens
import Text.Megaparsec hiding (State)

import Protolude hiding (catchJust, evalState, get, put, State)
import qualified Data.Graph as G
import qualified Data.List as L
import qualified Data.Map.Strict as M hiding (mapMaybe)
import qualified Data.MultiMap as MM
import qualified Data.Set as S

data ProgramDeclaration = 
  ProgramDeclaration { _programName :: !Text
                     , _declaration :: !EURM
                     , _program     :: !Program }
makeLenses ''ProgramDeclaration

data ModuleData = ModuleData { _programs :: ![ProgramDeclaration] }
makeLenses ''ModuleData

data ModuleLoadingError = ParsingError (ParseErrorBundle Text Void) 
                        | DeclarationCompilationError CompilationError
                        | DuplicatedNames (Set Text)
                        | UndefinedReferences (Set Text)
                        | CyclicDependency Text Text

loadFromText :: Members '[Input Env, Error ModuleLoadingError] r
             => Text -> Sem r ModuleData
loadFromText text =
  do decls <- fromEither (parse declarations "" text) & mapError ParsingError
     let names = view U.name <$> decls
         duplicatedNames = findDuplicates names
     if not . S.null $ duplicatedNames
       then throw . DuplicatedNames $ duplicatedNames
       else do let nameToDecl = M.fromList $ L.zip names decls
               compilationOrder <- mapMaybe (`M.lookup` nameToDecl) <$> calculateCompilationOrder nameToDecl
               initialEnv <- input
               _programs <- compileDeclarations compilationOrder 
                              & evalState initialEnv
               return ModuleData {..} 

compileDeclarations :: forall r. Members '[Error ModuleLoadingError, State Env] r
                    => [EURM] -> Sem r [ProgramDeclaration]
compileDeclarations = foldlM (\ds d -> processDecl d <&> (: ds)) []
  where processDecl _declaration =
          (do let _programName = _declaration ^. U.name
              possibleProgram <- compileDeclaration _declaration
                                   & inputToState
                                   & runError
              case possibleProgram of
                Left err -> case err of
                              UndefinedProgram undefName -> throw $ CyclicDependency _programName undefName
                              _ -> throw $ DeclarationCompilationError err
                Right _program ->
                  do oldEnv <- get
                     put $ M.insert _programName _program oldEnv
                     pure ProgramDeclaration {..}) :: Sem r ProgramDeclaration

calculateCompilationOrder :: forall r. Members '[Input Env, Error ModuleLoadingError] r
                          => Map Text EURM -> Sem r [Text]
calculateCompilationOrder decls =
  do envNames <- input <&> M.keysSet
     let namesSet = M.keysSet decls
         nameToIndexMap = M.fromAscList $ L.zip (S.toAscList namesSet) [1..]
         indexToNameMap = M.fromAscList $ L.zip [1..] (S.toAscList namesSet)
     dependenciesEdges <- fmap (MM.toList . MM.fromMap . M.fromList . catMaybes) $ forM (snd <$> M.toList decls) $ \decl ->
                     do let freeVars = freeVariables decl
                            nonEnvFreeVars = freeVars `S.difference` envNames
                            undefinedReferences = nonEnvFreeVars `S.difference` namesSet
                        if S.null undefinedReferences
                          then return $ (,) <$> M.lookup (decl ^. U.name) nameToIndexMap
                                            <*> traverse (flip M.lookup nameToIndexMap) (S.toAscList nonEnvFreeVars)
                          else throw $ UndefinedReferences undefinedReferences 
     let dependenciesGraph = G.buildG (1, S.size namesSet) dependenciesEdges
         ordering = reverse $ G.topSort dependenciesGraph
     pure $ mapMaybe (flip M.lookup indexToNameMap) ordering


findDuplicates :: (Foldable f, Ord a) => f a -> Set a
findDuplicates items =
  let countMap = foldl' (flip $ M.alter (\curr -> ((+ 1) <$> curr) <|> Just (1 :: Int))) M.empty items 
  in M.keysSet . M.filter (> 1) $ countMap
