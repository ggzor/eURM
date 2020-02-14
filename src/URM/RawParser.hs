module URM.RawParser where

import URM.Core
import URM.Parsing

import Control.Monad (void)
import Control.Applicative ((<*))

import Data.Functor ((<&>))
import qualified Data.Vector as V
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type RawParser = Parsec Void String 

parseURM :: String -> Maybe Instructions
parseURM = parseMaybe simpleURM

simpleURM :: RawParser Instructions
simpleURM = spaceConsumer >> multipleURMStatements <* eof

multipleURMStatements :: RawParser Instructions
multipleURMStatements = V.fromList <$> many (lexeme urmStatement)

urmStatement :: RawParser URM
urmStatement = zeroStatement <|> successorStatement <|>
                                transferStatement <|> jumpStatement

zeroStatement, successorStatement, transferStatement, jumpStatement :: RawParser URM
zeroStatement      = genericStatement 'Z' 1 <&> (\[r]       -> Zero r)
successorStatement = genericStatement 'S' 1 <&> (\[r]       -> Successor r)
transferStatement  = genericStatement 'T' 2 <&> (\[s, d]    -> Transfer s d)
jumpStatement      = genericStatement 'J' 3 <&> (\[l, r, i] -> Jump l r i)

genericStatement :: Char -> Int -> RawParser [Int]
genericStatement c arity =
  do lexeme (char c)
     lexeme (char '(')
     params <- lexeme parameter `sepBy1` lexeme (char ',')
     lexeme (char ')')
     if length params /= arity
       then fail $ "Arity mismatch: Expected " ++ show arity ++ ", Actual is " ++ show (length params)
       else return params

parameter :: RawParser Int
parameter = read <$> some digitChar
