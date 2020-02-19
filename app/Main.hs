module Main where

import URM.Core
import URM.Interpreter
import URM.Parsing

import URM.Extended.Compiler
import URM.Extended.Parser (parseEURM, name, EURMParseError)

import Control.Lens

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error

import qualified Data.Vector as V

import Data.Bifunctor
import Data.Maybe
import Data.Map.Strict as M
import Data.Set as S
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Applicative ((<*>))
import Data.Void
import System.IO
import System.Directory

data InterpreterState = 
  InterpreterState { _programs :: !(Map String Instructions)
                   , _compilerOptions :: !CompilerOptions
                   , _loadedFiles :: ![String] }
makeLenses ''InterpreterState

main :: IO ()
main = () <$ runMaybeT (evalStateT mainInterpreter (InterpreterState M.empty (CompilerOptions False) []))

mainInterpreter :: StateT InterpreterState (MaybeT IO) ()
mainInterpreter =
  do liftIO $ putStr "> " >> hFlush stdout
     line <- liftIO getLine
     case parse interpreterStatement "" line of
       Left error -> liftIO (putStr (errorBundlePretty error)) >> mainInterpreter
       Right action ->
        case action of
          ReplStatement Quit -> liftIO (putStrLn "Bye, bye") >> lift (fail "")
          ReplStatement Reload -> do liftIO $ putStrLn "Reloading files..."
                                     programs .= M.empty
                                     currentLoadedFiles <- use loadedFiles
                                     loadedFiles .= []
                                     mapM_ loadFile currentLoadedFiles
                                     mainInterpreter
          ReplStatement (SetCompilerFlag value flag) ->
            do case flag of 
                 Optimizations -> compilerOptions.enableOptimizations .= value
               mainInterpreter
          ReplStatement (ViewCode programName) ->
            do currentPrograms <- use programs
               case M.lookup programName currentPrograms of
                 Nothing -> liftIO $ putStrLn ("Unknown program name " ++ programName)
                 Just instructions -> do liftIO $ putStrLn (programName ++ ":")
                                         liftIO $ putStrLn
                                                    . unlines 
                                                    . fmap (\(n, i) -> "  "
                                                                     ++ (replicate (length (show (V.length instructions)) - length (show n)) ' ')
                                                                     ++ show n
                                                                     ++ ". " ++ i)
                                                    . fmap (second show)
                                                    $ zip [1..] (V.toList instructions)
               mainInterpreter
          ReplStatement (DumpCode programName path) ->
            do currentPrograms <- use programs
               case M.lookup programName currentPrograms of
                 Nothing -> liftIO $ putStrLn ("Unknown program name: " ++ programName)
                 Just program -> liftIO $ writeFile path (unlines $ show <$> V.toList program)
               mainInterpreter
          ReplStatement (Load path) -> loadFile path >> mainInterpreter
          EvaluateStatement programName params ->
            do currentPrograms <- use programs
               -- TODO: Arity warnings
               -- TODO: Maximum instruction count
               case M.lookup programName currentPrograms of
                 Nothing -> liftIO $ putStrLn ("Unknown program name: " ++ programName)
                 Just program -> liftIO $ putStrLn $ maybe 
                                                       ("The program didn't end after " ++ show maxInstructions ++ " instructions") 
                                                       show
                                                       (evaluate maxInstructions program mappedParams)
                                   where mappedParams = M.fromAscList $ zip [1..] params
                                         maxInstructions = 100000
               mainInterpreter

loadFile :: String -> StateT InterpreterState (MaybeT IO) ()
loadFile path = 
  do contents <- liftIO ((<|>) <$> readContentsIfExists path <*> readContentsIfExists (path ++ ".eurm"))
     case contents of
       Nothing -> liftIO $ putStrLn ("Unable to find file " ++ path)
       Just contents -> do liftIO $ putStrLn $ "Loading file " ++ path
                           currentPrograms <- use programs
                           options <- use compilerOptions
                           case parseEURMFile currentPrograms options contents of
                             Left (SyntaxError error) -> 
                               do liftIO $ putStrLn "Syntax error:"
                                  liftIO $ putStr (errorBundlePretty error)
                             Left (CompilationError error) ->
                               do liftIO $ putStrLn "Compilation error:"
                                  liftIO $ print error
                             Right newPrograms ->
                                 do let newNames = M.keysSet newPrograms
                                    let joinedNames names = unlines ["  - " ++ name | name <- S.toList names]
                                    programs .= newPrograms
                                    loadedFiles %= (++ [path])
                                    liftIO $ putStrLn $ "Successfully loaded " ++ show (S.size newNames) ++ " new programs:"
                                    liftIO $ putStrLn (joinedNames newNames)

readContentsIfExists :: String -> IO (Maybe String)
readContentsIfExists path =
  do exists <- doesFileExist path
     if exists
       then Just <$> readFile path
       else return Nothing

data EURMFileError = SyntaxError EURMParseError | CompilationError EURMCompilationError

parseEURMFile :: Env -> CompilerOptions -> String -> Either EURMFileError (Map String Instructions)
parseEURMFile env options contents = first SyntaxError (parseEURM contents)
                                       >>= (first CompilationError . compileEURM env options)

{-
Grammar
  <statement> ::= <repl-statement> | <evaluate-program-statement>
  
  <repl-statement> ::= ':' (<repl-load-statement> | <repl-quit-statement>)
  <repl-load-statement> ::= 'load' <file-path>
  <repl-quit-statement> ::= 'quit'
  
  <evaluate-program-statement> ::= <name> { <parameter> }
-}

data CompilerFlag = Optimizations
data ReplAction = Load String | DumpCode String String | SetCompilerFlag Bool CompilerFlag | ViewCode String | Reload | Quit
data InterpreterStatement = ReplStatement ReplAction | EvaluateStatement String [Int]


type MainParser = Parsec Void String
interpreterStatement = (replStatement <|> evaluateProgramStatement) <* spaceConsumer <* eof

evaluateProgramStatement :: MainParser InterpreterStatement
evaluateProgramStatement = EvaluateStatement <$> lexeme (some alphaNumChar) <*> decimal `sepEndBy` space1

replStatement :: MainParser InterpreterStatement
replStatement = char ':' >> ReplStatement <$> (replLoadStatement <|> replViewCodeStatement <|> replDumpCodeStatement
                                                <|> replSetCompilerFlag <|> replQuitStatement <|> replReloadStatement)
  where replLoadStatement = Load <$> (string "load" *> space1 *> takeRest)
        replViewCodeStatement = ViewCode <$> (string "view" *> space1 *> takeRest)
        replDumpCodeStatement = string "dump" *> space1 *> (DumpCode <$> name <*> takeRest)
        replQuitStatement = Quit <$ string "quit"
        replSetCompilerFlag = string "set" *> space1 *> (SetCompilerFlag <$> (("+" ==) <$> (string "+" <|> string "-")) <*> compilerFlag)
        replReloadStatement = Reload <$ string "reload"


compilerFlag :: MainParser CompilerFlag
compilerFlag = Optimizations <$ string "optimizations"
