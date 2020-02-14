module URM.Extended.Compiler where

import URM.Core
import URM.Extended.Core hiding (target)

import Data.Functor ((<&>))
import Data.Maybe
import qualified Data.Vector as V
import Control.Applicative
import Data.Foldable (asum)
import Control.Monad (join)
import Control.Monad.Trans.State
import Control.Lens

import Data.Map.Strict as M

data EURMCompilationError = None

instance Show EURMCompilationError where
  show _ = "Compilation error"

type Env = Map String Instructions

data CompilerState = CompilerState { _loadedPrograms :: !Env }
makeLenses ''CompilerState

builtIns = Prelude.foldl (\env (name, body) -> M.insert name (body env) env) M.empty 
            [("__builtin_successor", successor), ("__builtin_sum", sum)]
  where successor = const . V.singleton $ Successor 1
        sum = const . V.fromList $ 
                [ Jump 2 3 5
                , Successor 1
                , Successor 3
                , Jump 1 1 1]

removeBuiltIns = M.delete "__builtin_successor" . M.delete "__builtin_sum"

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
                                , x ^? _BoundedSumDeclaration >>= fromBoundedSumDeclaration env]
             newProgram & maybe (pure ()) (\instructions -> 
               loadedPrograms %= M.insert newProgramName (standarizeURM instructions))
             processPrograms xs

type BoundedSumDeclarationFields = (String, [Variable], Variable, Expression, Expression)
fromBoundedSumDeclaration :: Env -> BoundedSumDeclarationFields -> Maybe Instructions 
fromBoundedSumDeclaration env (name, parameters, index, top, body) =
  let indexedParams = M.fromList $ zip parameters [1..]
      namedParams = Name <$> parameters
      auxiliarProgramName = "__recursiveBoundedSum"
      auxiliarRecursiveStep = Call "__builtin_sum" [Call auxiliarProgramName (namedParams ++ [Name index]), body]
      auxiliarProgram = fromRecursiveDeclaration env
                          (auxiliarProgramName, parameters, index, Constant 0, auxiliarRecursiveStep)
      modifiedEnv = (\p -> env & at auxiliarProgramName ?~ p) <$> auxiliarProgram
      finalExpression = Call auxiliarProgramName (namedParams ++ [top])                      
  in join $ expressionToProgram indexedParams <$> modifiedEnv <*> pure finalExpression

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
  in  recurseURM n <$> baseCaseProgram <*> recursiveStepProgram

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
     return $ composeURM (M.size variables) program parametersPrograms

standarizeURM :: Instructions -> Instructions
standarizeURM program = program & traversed._Jump._3 %~ min (V.length program + 1)

-- Assumes top is standarized
concatURM :: Instructions -> Instructions -> Instructions
concatURM top bottom = top V.++ offsettedBottom
  where offsettedBottom = bottom & traversed._Jump._3 +~ V.length top

-- FIXME: Optimizable with Max functor
ro :: Instructions -> Int
ro = fromMaybe 0 . maximumOf (folded.folding
        (\i -> foldMap (i ^..) [register, source, target, left, right]))

urmConstant :: Int -> Instructions
urmConstant i = V.fromList $ Zero 1 : (Successor 1 <$ [1..i])

urmProject :: Int -> Instructions
urmProject i = V.singleton $ Transfer i 1

-- FIXME: Use proper vector functions
-- Assumes program is standarized
urmCall :: [Int] -> Int -> Instructions -> Instructions
urmCall parameters result program = 
  V.fromList (
              (uncurry Transfer <$> zip parameters [1..]) 
  ++          (Zero <$> [length parameters + 1 .. ro program]))
  `concatURM`  program 
  `V.snoc`     Transfer 1 result

-- Assumes f and gs are standarized
composeURM :: Int -> Instructions -> [Instructions] -> Instructions
composeURM n f gs =
  let k = length gs
      m = maximum ([n, k, ro f] ++ fmap ro gs)
  in Prelude.foldl concatURM 
       (V.fromList (uncurry Transfer <$> zip [1..] [m + 1 .. m + n]))
       (uncurry            (urmCall [m + 1 .. m + n]) <$> zip [m + n + 1 .. m + n + k] gs)
       `concatURM`          urmCall [m + n + 1 .. m + n + k] 1 f

recurseURM :: Int -> Instructions -> Instructions -> Instructions
recurseURM n baseCase recursiveStep =
  let m = maximum [n + 2, ro baseCase, ro recursiveStep]
      firstPart = 
        V.fromList 
        (uncurry      Transfer <$> zip [1..] [m + 1 .. m + n + 1])
        `concatURM`   urmCall [1..n] (m + n + 3) baseCase
      stopCondition = Jump (m + n + 1) (m + n + 2) (V.length firstPart + 1 + V.length lastPart)
      lastPart =      urmCall ([m + 1 .. m + n] ++ [m + n + 2, m + n + 3]) (m + n + 3) recursiveStep
        `V.snoc`      Successor (m + n + 2)
        `V.snoc`      Jump 1 1 (V.length firstPart - V.length firstPart)
        `V.snoc`      Transfer (m + n + 3) 1
  in  firstPart `V.snoc` stopCondition `concatURM` lastPart
