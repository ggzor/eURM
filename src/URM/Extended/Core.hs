{-# LANGUAGE NoImplicitPrelude #-}
module URM.Extended.Core where

import URM.Simple.Core

import Control.Lens

import Protolude

type InstructionsSeq = Seq URM
type Variable = Text

data Expression = Constant !Int 
                | Name !Text
                | Call !Text ![Expression]

data EURM = 
    RawDeclaration 
      { _name :: !Text
      , _code :: !InstructionsSeq }
  | RecursiveDeclaration 
      { _name             :: !Text
      , _nonRecursiveVars :: ![Variable]
      , _recursiveVar     :: !Variable
      , _baseCase         :: !Expression
      , _recursiveStep    :: !Expression }
  | CompositeDeclaration 
      { _name       :: !Text
      , _parameters :: ![Variable]
      , _body       :: !Expression }
  | AliasDeclaration 
      { _name   :: !Text
      , _target :: !Variable }
  | BoundedMinimizationDeclaration 
      { _name       :: !Text
      , _parameters :: ![Variable]
      , _index      :: !Variable
      , _top        :: !Expression
      , _body       :: !Expression }
  | BoundedSumDeclaration
      { _name       :: !Text
      , _parameters :: ![Variable]
      , _index      :: !Variable
      , _top        :: !Expression
      , _body       :: !Expression }
  | BoundedProductDeclaration
      { _name       :: !Text
      , _parameters :: ![Variable]
      , _index      :: !Variable
      , _top        :: !Expression
      , _body       :: !Expression }

makePrisms ''EURM
makeLenses ''EURM
