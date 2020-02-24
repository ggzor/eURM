{-# LANGUAGE NoImplicitPrelude #-}
module URM.Extended.Parser where

import URM.Extended.Core hiding (name)
import URM.Simple.Parser
import URM.Parsing

import Control.Lens
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Protolude hiding (many, try)
import Data.Sequence
import Data.Text

type URMExtendedParser = Parsec Void Text

name :: URMExtendedParser Text
name = (lexeme . fmap pack $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')) <?> "name"

expression, constant, call :: URMExtendedParser Expression
expression = constant <|> call <|> (Name <$> name)

constant = lexeme $ Constant <$> L.decimal

call = do callName <- try $ name <* symbol "("
          params <- expression `sepBy1` symbol "," <?> "call parameters"
          _ <- symbol ")"
          return $ Call callName params

declarations :: URMExtendedParser [EURM]
declarations = spaceConsumer >> many declaration

-- Order matters due to backtracking
declaration :: URMExtendedParser EURM
declaration = choice [ rawDeclaration
                     , aliasDeclaration
                     , recursiveDeclaration
                     , boundedMinimizationDeclaration
                     , boundedSumDeclaration
                     , boundedProductDeclaration
                     , compositeDeclaration ]

rawDeclaration, aliasDeclaration, compositeDeclaration :: URMExtendedParser EURM
recursiveDeclaration :: URMExtendedParser EURM
boundedMinimizationDeclaration, boundedSumDeclaration, boundedProductDeclaration :: URMExtendedParser EURM

rawDeclaration = do decName <- try $ name <* symbol ":"
                    RawDeclaration decName . fromList <$> instructions

aliasDeclaration = AliasDeclaration <$> try (name <* symbol "=") <*> name

compositeDeclaration = 
  do (_name, _parameters) <- parametrizedDeclaration
     _ <- symbol "="
     _body                <- expression              <?> "body"
     return $ CompositeDeclaration {..}

recursiveDeclaration = 
  do (_name, _nonRecursiveVars) <- 
       try $ (,) <$> (name                                       <?> "declaration name")
                 <*> (symbol "(" >> (name `sepEndBy` symbol ",") <?> "non-recursive variables")
                 <*  symbol "0"
     symbolsText ")="
     let repeatedFirstPart = symbols [_name, "("] <* forM_ _nonRecursiveVars (\v -> symbols [v, ","])
     _baseCase      <- expression  <?> "base case expression"
     repeatedFirstPart
     _recursiveVar  <- name        <?> "recursive variable"
     symbolsText "+1)="
     _recursiveStep <- expression  <?> "recursive case expression"
     return RecursiveDeclaration {..}

boundedMinimizationDeclaration = boundedDeclaration "μ" _BoundedMinimizationDeclaration
boundedSumDeclaration          = boundedDeclaration "Σ" _BoundedSumDeclaration
boundedProductDeclaration      = boundedDeclaration "Π" _BoundedProductDeclaration

boundedDeclaration :: Text -> AReview b (Text, [Text], Text, Expression, Expression) -> URMExtendedParser b
boundedDeclaration identifier constructor =
  do (_name, _parameters) <- try $ parametrizedDeclaration <* symbols ["=", identifier]
     _index <- name                              <?> "index variable"
     _      <- symbol "<"
     _top   <- expression                        <?> "top expression"
     _body  <- betweenSquareBrackets (expression <?> "body")
     return $ constructor # (_name, _parameters, _index, _top, _body)

parametrizedDeclaration :: URMExtendedParser (Text, [Text])
parametrizedDeclaration = (,) <$> (name                                     <?> "declaration name")
                              <*> betweenParens ((name `sepBy1` symbol ",") <?> "parameters")
