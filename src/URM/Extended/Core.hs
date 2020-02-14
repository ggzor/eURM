module URM.Extended.Core where

import URM.Core

import Control.Lens

type Variable = String

data Expression = Constant !Int 
                | Name !String
                | Call !String ![Expression]
                deriving (Show, Eq)

data EURM = 
    RawDeclaration 
      { _name :: !String
      , _code :: !Instructions }
  | RecursiveDeclaration 
      { _name             :: !String
      , _nonRecursiveVars :: ![Variable]
      , _recursiveVar     :: !Variable
      , _baseCase         :: !Expression
      , _recursiveStep    :: !Expression }
  | CompositeDeclaration 
      { _name       :: !String
      , _parameters :: ![Variable]
      , _body       :: !Expression }
  | AliasDeclaration 
      { _name   :: !String
      , _target :: !Variable }
  | MinimizationDeclaration 
      { _name       :: !String
      , _parameters :: ![Variable]
      , _index      :: !Variable
      , _top        :: !Expression
      , _predicate  :: !Expression }
  | BoundedSumDeclaration
      { _name       :: !String
      , _parameters :: ![Variable]
      , _index      :: !Variable
      , _top        :: !Expression
      , _body       :: !Expression }
  deriving Show

makePrisms ''EURM
makeLenses ''EURM
