module URM.Extended.Parser where

import URM.RawParser (multipleURMStatements)
import URM.Extended.Core hiding (name)
import URM.Parsing

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Text.Megaparsec.Error

import Data.Void

type Parser = Parsec Void String
type EURMParseError = ParseErrorBundle String Void

parseEURM :: String -> Either EURMParseError [EURM]
parseEURM = parse multipleDeclarations ""

multipleDeclarations :: Parser [EURM]
multipleDeclarations = spaceConsumer >> many (lexeme declaration)

declaration :: Parser EURM
declaration = choice [rawDeclaration, recursiveDeclaration, minimizationDeclaration, boundedSumDeclaration,
                        compositeDeclaration, aliasDeclaration ]

rawDeclaration, recursiveDeclaration, 
  compositeDeclaration, aliasDeclaration, minimizationDeclaration :: Parser EURM

name :: Parser String
name = lexeme $ (:) <$> letterChar <*> many alphaNumChar

rawDeclaration = do decName <- try $ name <* symbol ":"
                    RawDeclaration decName <$> multipleURMStatements
                    
recursiveDeclaration = 
  do (_name, _nonRecursiveVars) <- 
        try $ do decName <- name
                 symbol "("
                 vars <- name `sepEndBy` symbol ","
                 symbol "0"
                 return (decName, vars)
     symbols ")="
     _baseCase <- expression
     symbol _name
     symbol "("
     mapM_ (\v -> symbol v >> symbol ",") _nonRecursiveVars
     _recursiveVar <- name
     symbols "+1)="
     _recursiveStep <- expression
     return RecursiveDeclaration {..}
     

expression :: Parser Expression
expression = constant <|> call <|> (Name <$> name)

constant :: Parser Expression
constant = lexeme $ Constant <$> L.decimal

call :: Parser Expression
call = do callName <- try $ name <* symbol "("
          parameters <- expression `sepBy1` symbol ","
          symbol ")"
          return $ Call callName parameters

compositeDeclaration = 
  do _name <- try $ name <* symbol "("
     _parameters <- name `sepBy1` symbol "," 
     symbols ")="
     _body <- expression
     return $ CompositeDeclaration {..}

aliasDeclaration = AliasDeclaration <$> name <* symbol "=" <*> name

minimizationDeclaration =
  do (_name, _parameters) <- try $ do decName <- name 
                                      symbol "("
                                      parameters <- name `sepBy1` symbol ","
                                      symbols ")="
                                      string "μ"
                                      return (decName, parameters)
     _index <- name
     symbol "<"
     _top <- expression
     _predicate <- between (symbol "[") (symbol "]") expression
     return $ MinimizationDeclaration {..}

boundedSumDeclaration =
  do (_name, _parameters) <- try $ do decName <- name 
                                      symbol "("
                                      parameters <- name `sepBy1` symbol ","
                                      symbols ")="
                                      string "Σ"
                                      return (decName, parameters)
     _index <- name
     symbol "<"
     _top <- expression
     _body <- between (symbol "[") (symbol "]") expression
     return $ BoundedSumDeclaration {..}
