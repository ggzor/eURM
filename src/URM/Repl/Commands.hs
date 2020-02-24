{-# LANGUAGE NoImplicitPrelude #-}
module URM.Repl.Commands where

import URM.Repl.Options

import URM.Parsing
import qualified URM.Extended.Parser as U

import Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Polysemy
import Polysemy.Error

import Protolude
import Data.Text (pack)

type ProgramName = Text
type ProgramInputs = [Int]
type ModuleName = Text

data ReplCommand = SetOptions ReplOptions
                 | Trace ProgramName ProgramInputs
                 | Evaluate ProgramName ProgramInputs
                 | View ProgramName
                 | Unload ModuleName 
                 | Load Text
                 | Reload 
                 | Quit

type ReplCommandParserError = Void

parseCommand :: Member (Error (ParseErrorBundle Text ReplCommandParserError)) r 
             => Text -> Sem r ReplCommand
parseCommand command = fromEither $ parse replCommand "" command

type ReplCommandParser = Parsec ReplCommandParserError Text

replCommand, specialCommand :: ReplCommandParser ReplCommand
replCommand = specialCommand <|> evaluateCommand

evaluateCommand :: ReplCommandParser ReplCommand
evaluateCommand = Evaluate <$> U.name <*> M.many (lexeme L.decimal)

specialCommand = string ":" *> choice 
                                  (  ([(Reload, "reload"), (Quit, "quit")] <&> uncurry simpleCommand)
                                  ++ ([(View, "view"), (Load, "load")] <&> uncurry oneTextParameterCommand))

oneTextParameterCommand :: (Text -> ReplCommand) -> Text -> ReplCommandParser ReplCommand
oneTextParameterCommand constructor name = 
  constructor <$> (string name *> spaceChar *> (pack <$> M.some printChar))

simpleCommand :: ReplCommand -> Text -> ReplCommandParser ReplCommand
simpleCommand command name = command <$ lexeme (string name)
