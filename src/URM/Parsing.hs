{-# LANGUAGE NoImplicitPrelude #-}
module URM.Parsing where

import Text.Megaparsec
import Text.Megaparsec.Internal
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Protolude
import Data.String
import Data.Text hiding (empty)

spaceConsumer :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ()
spaceConsumer = L.space space1 (L.skipLineComment "#") empty

lexeme :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m a -> m a
lexeme = L.lexeme spaceConsumer

symbol :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => Tokens s -> m (Tokens s)
symbol = L.symbol spaceConsumer

symbols :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => [Tokens s] -> m () 
symbols = mapM_ (lexeme . symbol)

symbolsText :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => String -> m ()
symbolsText = symbols . fmap singleton

symbolsString :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => String -> m ()
symbolsString = symbols . fmap (: [])

betweenParens :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m a -> m a
betweenParens = between (symbol "(") (symbol ")")

betweenSquareBrackets :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m a -> m a
betweenSquareBrackets = between (symbol "[") (symbol "]")

mapParserError :: forall e1 e2 s m a. (Ord e1, Ord e2) => (e1 -> e2) -> ParsecT e1 s m a -> ParsecT e2 s m a
mapParserError f m = 
  ParsecT $ \s cok cerr eok eerr ->
              let mcok = cerr . mapParseError f
                  meok = eerr . mapParseError f
              in unParser m s cok mcok eok meok
