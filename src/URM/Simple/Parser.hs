{-# LANGUAGE NoImplicitPrelude #-}
module URM.Simple.Parser where

import URM.Simple.Core
import URM.Parsing

import Fmt

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Protolude hiding (many)
import Control.Monad (fail)
import Data.Text (unpack)

type URMSimpleParser = Parsec Void Text

instructions :: URMSimpleParser [URM]
instructions = many (lexeme instruction) <?> "simple urm instructions"

instruction :: URMSimpleParser URM
instruction = (  zeroInstruction 
             <|> successorInstruction
             <|> transferInstruction
             <|> jumpInstruction)     <?> "simple urm instruction"

zeroInstruction, successorInstruction, transferInstruction, jumpInstruction :: URMSimpleParser URM
zeroInstruction      = genericInstruction 'Z' 1 <&> (\[r]       -> Zero r)       <?> "zero instruction"
successorInstruction = genericInstruction 'S' 1 <&> (\[r]       -> Successor r)  <?> "successor instruction"
transferInstruction  = genericInstruction 'T' 2 <&> (\[s, d]    -> Transfer s d) <?> "transfer instruction"
jumpInstruction      = genericInstruction 'J' 3 <&> (\[l, r, i] -> Jump l r i)   <?> "jump instruction"

genericInstruction :: Char -> Int -> URMSimpleParser [Int]
genericInstruction c arity =
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
