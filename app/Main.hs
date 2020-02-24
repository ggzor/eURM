{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import URM.Repl.Commands
import URM.Repl.Options

import URM.Simple.Core
import URM.Extended.Compiler
import URM.Extended.Core
import URM.Extended.Modules

import URM.Interpreter

import Control.Lens
import Control.Lens.Unsound

import Data.Text as T (intercalate, splitOn, unpack)
import Data.Foldable
import Text.Megaparsec (errorBundlePretty, ParseErrorBundle)

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.Input.Utils

import Protolude hiding (State, evalState, get, to, moduleName, intercalate, modify, evaluate)
import Data.Map.Strict as M
import Data.Set as S
import qualified Data.Vector as V
import qualified Prelude as P
import System.Directory
import System.IO (hFlush)

import Fmt

data CommandOutput = EvaluationResult !Int
                   | CompiledModule !ModuleDefinition 
                   | ReloadedModules ![ModuleDefinition]
                   | ViewCode !Program

data InputOutputError = UnexistentFile Text

data ReplError = ParseCommandError (ParseErrorBundle Text ReplCommandParserError)
               | ReplModuleLoadingError ModuleLoadingError
               | ReplIOError InputOutputError
               | ReloadError ReplError
               | StepsLimitError
               | ReplUndefinedProgram Text
               | QuitRepl

data ModuleDefinition = 
  ModuleDefinition { _file       :: !Text
                   , _moduleName :: !ModuleName
                   , _contents   :: !ModuleData }
makePrisms ''CommandOutput
makeLenses ''ModuleDefinition

data ReplState = 
  ReplState { _loadedModules :: [ModuleDefinition] 
            , _loadedFiles   :: Set Text }
makeLenses ''ReplState


defaultReplOptions :: ReplOptions
defaultReplOptions =
  ReplOptions { _interfaceOptions = 
                  InterfaceOptions { _prompt = "> " }
              , _executionOptions = 
                  ExecutionOptions { _maxSteps = 100000 } }

initialReplState :: ReplState
initialReplState = 
  ReplState { _loadedModules = [], _loadedFiles = S.empty }

main :: IO ()
main = repl 
     & evalState defaultReplOptions
     & evalState initialReplState
     & runM

repl :: Members '[Embed IO, State ReplOptions, State ReplState] r 
     => Sem r ()
repl = do result <- runError actions
          either (\case 
            QuitRepl -> embed $ putStrLn @Text "Quitting the REPL..."
            other -> (handleReplError other) & inputToState >> repl) (const repl) result
  where actions = readInput    & inputToState
              >>= parseCommand & mapError ParseCommandError
              >>= evalCommand  & inputToState @ReplState
                               & inputToState @ReplOptions
              >>= processOutput

handleReplError :: Members '[Embed IO, Input ReplOptions] r 
                => ReplError -> Sem r ()
handleReplError = \case
  ReplIOError replIOError -> case replIOError of
    UnexistentFile filePath -> embed $ putStrLn @Text $ "The file \""+|filePath|+"\" doesn't exist"
  ReplModuleLoadingError loadingError -> case loadingError of
    ParsingError parseError -> embed $ putStr @P.String $ errorBundlePretty parseError
    DeclarationCompilationError decCompilationError -> case decCompilationError of
      UnsupportedDeclarationType tag -> embed $ putStrLn @P.String $ "Unsupported declaration type: " ++ show tag
      InvalidRecursiveCall callName params -> 
        embed $ putStrLn @Text $   "There is an invalid recursive call in the definition of "+|callName|+".\n"
                               +|| "The only valid recursive call is: "+|callName|+"("+|", " `intercalate` params|+")"
      UndefinedProgram undefProgram -> embed $ putStrLn @Text $ "Undefined program: "+|undefProgram|+""
    DuplicatedNames dupNames -> embed $ putStrLn @Text $ fmt $ nameF "The following names are duplicated" $ blockListF dupNames
    UndefinedReferences undefNames -> embed $ putStrLn @Text $ fmt $ nameF "The following names aren't defined" $ blockListF undefNames
    CyclicDependency p1 p2 -> embed $ putStrLn @Text $ "There is a cyclic dependency between the following declarations: "+|p1|+" and "+|p2|+""
  ParseCommandError parseError -> embed $ putStr @P.String $ errorBundlePretty parseError
  ReloadError reloadError -> handleReplError reloadError
  ReplUndefinedProgram progName -> embed $ putStrLn @Text $ "Unknown program name: "+|progName|+""
  StepsLimitError -> 
    do stepsLimit <- input <&> view (executionOptions.maxSteps)
       embed $ putStrLn @Text $ "The program reached the steps limit"
  QuitRepl -> pure ()

readInput :: Members '[Input ReplOptions, Embed IO] r => Sem r Text
readInput = 
  do replOptions <- input <&> view (interfaceOptions.prompt)
     embed $ putStr replOptions >> hFlush stdout
     embed getLine

processOutput :: Members '[State ReplState, Embed IO] r
              => CommandOutput -> Sem r ()
processOutput = \case
  CompiledModule moduleDef ->
    do modify $ over loadedModules (moduleDef :)
       modify $ over loadedFiles (S.insert (moduleDef ^. file))
       embed $ putStrLn @Text $ "Successfully loaded module "+|moduleDef^.moduleName|+""
       embed $ putStrLn @Text $ fmt $ nameF ("Loaded "+|(moduleDef & lengthOf (contents.programs.folded))|+" new programs") $ blockListF $ moduleDef ^.. contents.programs.folded.declaration.to eurmAsText
  ReloadedModules modules ->
    do modify $ set loadedModules []
       forM_ modules (processOutput . CompiledModule)
  EvaluationResult result ->
    embed $ putStrLn @Text $ ""+|result|+""
  ViewCode code ->
    embed $ putStrLn @Text $ fmt $ indentF 4 $ unlinesF . fmap urmAsText $ V.toList code

evalCommand :: Members '[Embed IO, Error ReplError, Input ReplState, Input ReplOptions] r
            => ReplCommand -> Sem r CommandOutput
evalCommand = \case
  View name ->
    do programDecl <- searchReplProgram name
       return . ViewCode $ programDecl ^. program 
  Evaluate name inputs ->
    do stepsLimit <- input @ReplOptions <&> view (executionOptions.maxSteps)
       programDecl <- searchReplProgram name
       let initialState = M.fromList $ zip [1..] inputs
       case evaluate stepsLimit (programDecl ^. program) initialState of
         Nothing -> throw StepsLimitError
         Just evalResult -> return $ EvaluationResult evalResult
  Load filePath ->
    do (_file, fileContents) <- [filePath|+".eurm", filePath] 
                     &  traversed (\t -> runError $ (t,) <$> readFileContents t)
                    <&> foldl1 (<>)
                    <&> first ReplIOError
                    >>= fromEither
       let _moduleName = T.intercalate "." . Protolude.initDef ["main"] $ splitOn "." _file
       embed $ putStrLn @Text $ "Compiling module "+|_moduleName|+"..."
       _contents <- loadFromText fileContents
                      & mapReplStateToEnv
                      & mapError ReplModuleLoadingError
       return $ CompiledModule ModuleDefinition {..}
  Reload -> 
    do files <- input @ReplState <&> view loadedFiles <&> S.toList
       (ReloadedModules . catMaybes <$> forM files (fmap (preview _CompiledModule) . evalCommand . Load)) 
          & inputToState @Env
          & evalState M.empty
          & mapError ReloadError
  Quit -> throw QuitRepl
  _ -> undefined

readFileContents :: Members '[Embed IO, Error InputOutputError] r => Text -> Sem r Text
readFileContents filePath =
  do exists <- embed $ doesFileExist (unpack filePath)
     if exists 
       then embed $ readFile (unpack filePath)
       else throw (UnexistentFile filePath)

searchReplProgram :: Members '[Error ReplError, Input ReplState] r
                  => Text -> Sem r ProgramDeclaration
searchReplProgram programName =
  do targetProgram <- input <&> viewProgramDecls <&> M.lookup programName
     maybe (throw $ ReplUndefinedProgram programName) 
            return targetProgram

mapReplStateToEnv :: Member (Input ReplState) r => Sem (Input Env ': r) a -> Sem r a
mapReplStateToEnv = mapInput @Env @ReplState 
                          ( M.fromList 
                          . fmap (view $ lensProduct programName program) 
                          . toListOf (loadedModules.folded.contents.programs.folded))

viewProgramDecls = (M.fromList 
                          . fmap (view $ lensProduct programName identity) 
                          . toListOf (loadedModules.folded.contents.programs.folded))
