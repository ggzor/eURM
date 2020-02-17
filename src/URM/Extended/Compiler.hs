module URM.Extended.Compiler where

import URM.Core
import URM.Combination
import URM.Optimization.LowLevel
import URM.Extended.Core hiding (target)

import Data.Functor ((<&>))
import Data.Maybe
import qualified Data.Vector as V
import Control.Applicative
import Data.Foldable (asum)
import Control.Monad (join)
import Control.Monad.Trans.State
import Control.Lens

import GHC.IO.Unsafe

import Data.Map.Strict as M

data EURMCompilationError = None

instance Show EURMCompilationError where
  show _ = "Compilation error"

type Env = Map String Instructions

data CompilerState = CompilerState { _loadedPrograms :: !Env }
makeLenses ''CompilerState

builtIns = Prelude.foldl (\env (name, body) -> M.insert name (body env) env) M.empty 
            [ ("__builtin_successor", successor)
            , ("__builtin_signum", signum)
            , ("__builtin_sum", sum)
            , ("__builtin_product", product)]
  where successor = const . V.singleton $ Successor 1
        signum = const . V.fromList $
                   [ Jump 1 2 4
                   , Successor 2
                   , Transfer 2 1 ]
        sum = const . V.fromList $ 
                [ Jump 2 3 5
                , Successor 1
                , Successor 3
                , Jump 1 1 1]
        product env = fromMaybe undefined $ fromRecursiveDeclaration env
                        ("__builtin_product", ["x"], "y"
                            , Constant 0
                            , Call "__builtin_sum" [Call "__builtin_product" [Name "x", Name "y"], Name "x"])

removeBuiltIns = 
    M.delete "__builtin_successor" 
  . M.delete "__builtin_signum"
  . M.delete "__builtin_sum" 
  . M.delete "__builtin_product"

compileEURM :: [EURM] -> Either EURMCompilationError Env
compileEURM programs = Right $ removeBuiltIns (evalState (processPrograms programs) $ CompilerState builtIns)
  where processPrograms [] = use loadedPrograms
        processPrograms (x : xs) =
          do env <- use loadedPrograms
             let newProgramName = x ^. name
                 newProgram = asum
                                [ x ^? _RawDeclaration._2
                                , x ^? _CompositeDeclaration  >>= fromCompositeDeclaration env
                                , x ^? _AliasDeclaration._2   >>= flip M.lookup env
                                , x ^? _RecursiveDeclaration  >>= fromRecursiveDeclaration env
                                , x ^? _BoundedSumDeclaration >>= fromBoundedSumDeclaration env
                                , x ^? _BoundedProductDeclaration >>= fromBoundedProductDeclaration env
                                , x ^? _BoundedMinimizationDeclaration >>= fromBoundedMinimizationDeclaration env]
                 optimizations = Prelude.foldl (.) id [removeUselessTransfers]
             newProgram & maybe (pure ()) (\instructions -> 
               loadedPrograms %= M.insert newProgramName (optimizations . standarize $ instructions))
             processPrograms xs

type BoundedMinimizationDeclarationFields = (String, [Variable], Variable, Expression, Expression)
fromBoundedMinimizationDeclaration :: Env -> BoundedMinimizationDeclarationFields -> Maybe Instructions
fromBoundedMinimizationDeclaration env (name, parameters, index, top, body) =
  let innerIndexName = "__innerIndex"
      innerBody = Call "__builtin_signum" [replaceName index innerIndexName body]
      outerIndexName = "__outerIndex"
      productProgramName = "__innerProduct"
      productProgram = fromBoundedProductDeclaration env 
                         (productProgramName, parameters ++ [outerIndexName], innerIndexName
                         , Call "__builtin_successor" [Name outerIndexName]
                         , innerBody)
      sumProgramName = "__outerSum"
      modifiedEnv = (\p -> env & at productProgramName ?~ p) <$> productProgram
 in flip fromBoundedSumDeclaration
      ( sumProgramName
      , parameters, outerIndexName, top
      , Call productProgramName ((Name <$> parameters) ++ [Name outerIndexName])) =<< modifiedEnv

replaceName :: String -> String -> Expression -> Expression
replaceName original new (Name actual) | original == actual = Name new
replaceName original new (Call callName params) = Call callName (replaceName original new <$> params)
replaceName _ _ other = other

type BoundedProductDeclarationFields = (String, [Variable], Variable, Expression, Expression)
fromBoundedProductDeclaration :: Env -> BoundedProductDeclarationFields -> Maybe Instructions
fromBoundedProductDeclaration env (name, parameters, index, top, body) = 
  let namedParams = Name <$> parameters
      auxiliarProgramName = "__recursiveBoundedProduct"
      auxiliarRecursiveStep = Call "__builtin_product" [Call auxiliarProgramName (namedParams ++ [Name index]), body] 
      auxiliarProgram = fromRecursiveDeclaration env 
                          (auxiliarProgramName, parameters, index, Constant 1, auxiliarRecursiveStep)
      modifiedEnv = (\p -> env & at auxiliarProgramName ?~ p) <$> auxiliarProgram
      finalExpression = Call auxiliarProgramName (namedParams ++ [top])
  in flip fromCompositeDeclaration (name, parameters, finalExpression) =<< modifiedEnv

type BoundedSumDeclarationFields = (String, [Variable], Variable, Expression, Expression)
fromBoundedSumDeclaration :: Env -> BoundedSumDeclarationFields -> Maybe Instructions 
fromBoundedSumDeclaration env (name, parameters, index, top, body) =
  let namedParams = Name <$> parameters
      auxiliarProgramName = "__recursiveBoundedSum"
      auxiliarRecursiveStep = Call "__builtin_sum" [Call auxiliarProgramName (namedParams ++ [Name index]), body]
      auxiliarProgram = fromRecursiveDeclaration env
                          (auxiliarProgramName, parameters, index, Constant 0, auxiliarRecursiveStep)
      modifiedEnv = (\p -> env & at auxiliarProgramName ?~ p) <$> auxiliarProgram
      finalExpression = Call auxiliarProgramName (namedParams ++ [top])                      
  in flip fromCompositeDeclaration (name, parameters, finalExpression) =<< modifiedEnv

type CompositeDeclarationFields = (String, [Variable], Expression)
fromCompositeDeclaration :: Env -> CompositeDeclarationFields -> Maybe Instructions
fromCompositeDeclaration env (name, parameters, body) = 
  let indexedParams = M.fromList $ zip parameters [1..]
  in expressionToProgram indexedParams env body

type RecursiveDeclarationFields = (String, [Variable], Variable, Expression, Expression)

fromRecursiveDeclaration :: Env -> RecursiveDeclarationFields -> Maybe Instructions
fromRecursiveDeclaration env (name, nonRecursiveVars, recursiveVar, baseCase, recursiveStep) =
  let n = Prelude.length nonRecursiveVars
      baseCaseParams = M.fromList $ zip nonRecursiveVars [1..]
      recursiveResultVar = "__recursiveResult"
      recursiveStepParams = baseCaseParams & at recursiveVar       ?~ (n + 1)
                                           & at recursiveResultVar ?~ (n + 2)
      baseCaseProgram = expressionToProgram baseCaseParams env baseCase
      recursiveStepExpression = replaceRecursiveCalls name 
                                                      nonRecursiveVars recursiveVar recursiveResultVar
                                                      recursiveStep
      recursiveStepProgram = expressionToProgram recursiveStepParams env =<< recursiveStepExpression
  in  recurse n <$> baseCaseProgram <*> recursiveStepProgram

replaceRecursiveCalls :: String -> [Variable] -> Variable -> Variable -> Expression -> Maybe Expression
replaceRecursiveCalls name extraVars recursiveVar specialVarName expr = 
  let parametersToMatch = (Name <$> extraVars) ++ [Name recursiveVar]
  in case expr of
       Call callName params | callName == name ->
         if parametersToMatch == params
           then Just $ Name specialVarName
           else Nothing
       Call callName params ->
         do newParams <- traverse 
                           (replaceRecursiveCalls name extraVars recursiveVar specialVarName) 
                           params
            return $ Call callName newParams
       other -> Just other

expressionToProgram ::Map Variable Int -> Env -> Expression -> Maybe Instructions
expressionToProgram _ _ (Constant i) = Just $ urmConstant i
expressionToProgram variables programs (Name name) =
  (urmProject <$> M.lookup name variables) <|> M.lookup name programs
expressionToProgram variables programs (Call name params) =
  do program <- M.lookup name programs
     parametersPrograms <- traverse (expressionToProgram variables programs) params
     return $ compose (M.size variables) program parametersPrograms
