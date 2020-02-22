{-# LANGUAGE NoImplicitPrelude #-}
module URM.Simple.Parser where

import URM.Simple.Core
import URM.Parsing

import Fmt

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Protolude
import Control.Monad (fail)
import Data.Text (unpack)

type URMSimpleParser = Parsec Void Text

urmStatement :: URMSimpleParser URM
urmStatement = (  zeroStatement 
              <|> successorStatement
              <|> transferStatement
              <|> jumpStatement) <?> "simple urm statement"

zeroStatement, successorStatement, transferStatement, jumpStatement :: URMSimpleParser URM
zeroStatement      = genericStatement 'Z' 1 <&> (\[r]       -> Zero r)       <?> "zero statement"
successorStatement = genericStatement 'S' 1 <&> (\[r]       -> Successor r)  <?> "successor statement"
transferStatement  = genericStatement 'T' 2 <&> (\[s, d]    -> Transfer s d) <?> "transfer statement"
jumpStatement      = genericStatement 'J' 3 <&> (\[l, r, i] -> Jump l r i)   <?> "jump statement"

genericStatement :: Char -> Int -> URMSimpleParser [Int]
genericStatement c arity =
  do _ <- lexeme (char c)
     _ <- lexeme (char '(')
     params <- lexeme parameter `sepBy1` lexeme (char ',')
     _ <- lexeme (char ')')
     if length params /= arity
       then fail . unpack $ "Arity mismatch: Expected "+|arity|+", Actual is "+|length params|+""
       else return params

parameter :: URMSimpleParser Int
parameter =
  (do number <- L.decimal
      if number == 0
        then fail . unpack $ "The parameter has to be greater than zero"
        else return number) <?> "parameter"
