{-# LANGUAGE NoImplicitPrelude #-}
module URM.Extended.Compiler where

import URM.Simple.Core
import URM.Extended.BuiltIns
import URM.Extended.Combination
import URM.Extended.Core

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Input.Utils

import Control.Lens

import Protolude hiding (Reader, asks, State, get, runReader)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Vector.Utils

type Env = Map Text InstructionsVec
data CompilationError = UnsupportedDeclarationType EURMTag | InvalidRecursiveCall Text [Variable] | UndefinedProgram Text
type CompilationUnit r = Members '[Input Env, Error CompilationError] r
type InstructionsVec = V.Vector URM

data DeclarationCompiler r = forall a. DeclarationCompiler (Prism' EURM a)  (a -> Sem r InstructionsVec)

compileDeclaration :: forall r. CompilationUnit r => EURM -> Sem r InstructionsVec
compileDeclaration dec =
  do let declarationCompilers = 
           [ DeclarationCompiler _RawDeclaration       compileRawDeclaration
           , DeclarationCompiler _AliasDeclaration     compileAliasDeclaration
           , DeclarationCompiler _CompositeDeclaration compileCompositeDeclaration
           , DeclarationCompiler _RecursiveDeclaration compileRecursiveDeclaration] :: [DeclarationCompiler r]
     let compilationResult = foldr' (\(DeclarationCompiler deconstructor compiler) result -> 
                                         (compiler <$> dec ^? deconstructor) <|> result)
                                    Nothing declarationCompilers
     fromMaybe (throw . UnsupportedDeclarationType $ getEURMTag dec) compilationResult

compileRawDeclaration       :: CompilationUnit r => RawDeclarationFields       -> Sem r InstructionsVec
compileAliasDeclaration     :: CompilationUnit r => AliasDeclarationFields     -> Sem r InstructionsVec
compileCompositeDeclaration :: CompilationUnit r => CompositeDeclarationFields -> Sem r InstructionsVec
compileRecursiveDeclaration :: CompilationUnit r => RecursiveDeclarationFields -> Sem r InstructionsVec

compileRawDeclaration       (_, decCode)            = pure . fromSeq $ decCode
compileAliasDeclaration     (_, decTarget)          = searchProgram decTarget
compileCompositeDeclaration (_, decParams, decBody) = compileExpressionWithParameters decParams decBody

compileRecursiveDeclaration (decName, decNonRecVars, decRecVar, decBaseCase, decRecStep) =
  do let recResultVar = "_recursiveResult"
     baseCaseProgram       <- compileExpressionWithParameters decNonRecVars decBaseCase
     replacedRecursiveStep <- replaceRecursiveCalls decName decNonRecVars decRecVar recResultVar decRecStep
     recursiveStepProgram  <- compileExpressionWithParameters 
                                (decNonRecVars ++ [decRecVar, recResultVar])
                                replacedRecursiveStep
     return . fromSeq $ recurse (length decNonRecVars) baseCaseProgram recursiveStepProgram

searchProgram :: CompilationUnit r => Text -> Sem r InstructionsVec
searchProgram programName = 
  do targetProgram <- input <&> M.lookup programName
     maybe (throw $ UndefinedProgram programName) 
            return targetProgram

compileExpression :: CompilationUnit r => Expression -> Sem r InstructionsVec
compileExpression (Constant i) = return $ constant i
compileExpression (Name decName) = searchProgram decName
compileExpression (Call decName decParameters) =
  do program <- searchProgram decName
     parameterPrograms <- traverse compileExpression decParameters
     return . fromSeq $ compose (length parameterPrograms) program parameterPrograms

compileExpressionWithParameters :: CompilationUnit r => [Variable] -> Expression -> Sem r InstructionsVec
compileExpressionWithParameters params expr =
  let newEnv = M.union . M.fromList . fmap (second project) $ zip params [1..]
  in compileExpression expr & mapInput newEnv

replaceRecursiveCalls :: CompilationUnit r => Text -> [Variable] -> Variable -> Variable -> Expression -> Sem r Expression
replaceRecursiveCalls decName nonRecVars recVar resultVar expr =
  do let parametersToMatch = (Name <$> nonRecVars) ++ [Name recVar]
     case expr of
       Call callName params | callName == decName ->
         if params == parametersToMatch
           then return $ Name resultVar
           else throw  $ InvalidRecursiveCall decName (parametersToMatch ^.. folded._Name)
       Call callName params ->
         do newParams <- traverse (replaceRecursiveCalls decName nonRecVars recVar resultVar) params
            return $ Call callName newParams
       other -> return other
