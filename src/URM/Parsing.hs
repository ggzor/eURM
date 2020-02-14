module URM.Parsing where

import Data.String
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

spaceConsumer :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ()
spaceConsumer = L.space space1 (L.skipLineComment "#") empty

lexeme :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m a -> m a
lexeme = L.lexeme spaceConsumer

symbol :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => Tokens s -> m (Tokens s)
symbol = L.symbol spaceConsumer

symbols :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => [Token s] -> m [[Token s]]
symbols = mapM (symbol . (: []))
