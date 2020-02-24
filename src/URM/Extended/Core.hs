{-# LANGUAGE NoImplicitPrelude #-}
module URM.Extended.Core where

import URM.Simple.Core
import URM.TH

import Control.Lens hiding (index)
import Fmt

import Protolude hiding (empty, intercalate, to)
import Data.Text hiding (empty, index, null, singleton)
import Data.Set hiding (null, toList)

type InstructionsSeq = Seq URM
type Variable = Text

data Expression = Constant !Int 
                | Name !Text
                | Call !Text ![Expression]
                deriving (Eq)
makePrisms ''Expression

data EURM = 
    RawDeclaration 
      { _name :: !Text
      , _code :: !InstructionsSeq }
  | AliasDeclaration 
      { _name   :: !Text
      , _target :: !Variable }
  | CompositeDeclaration 
      { _name       :: !Text
      , _parameters :: ![Variable]
      , _body       :: !Expression }
  | RecursiveDeclaration 
      { _name             :: !Text
      , _nonRecursiveVars :: ![Variable]
      , _recursiveVar     :: !Variable
      , _baseCase         :: !Expression
      , _recursiveStep    :: !Expression }
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
makeFieldNames ''EURM
makeTags ''EURM
deriving instance Show EURMTag

variables :: Expression -> Set Variable
variables = \case 
  (Constant _)           -> empty
  (Name expName)         -> singleton expName
  (Call callName params) -> unions $ singleton callName : (variables <$> params)

freeVariables :: EURM -> Set Variable
freeVariables = \case 
  (RawDeclaration _ _)                 -> empty
  (AliasDeclaration _ decTarget)       -> singleton decTarget
  (CompositeDeclaration _ params decBody) -> variables decBody `difference` fromList params
  (RecursiveDeclaration decName params recVar decBaseCase recStep) -> 
    (variables decBaseCase `difference` fromList params) `union` (variables recStep `difference` fromList (decName : recVar : params))
  bounded -> 
    let params   = fromList $ bounded ^.. parameters.folded
        topVars  = fromMaybe empty $ bounded ^? top.to variables
        bodyVars = fromMaybe empty $ bounded ^? body.to variables
        indexVar = fromList $ bounded ^.. index
    in (topVars `difference` params) `union` (bodyVars `difference` (params `union` indexVar))

expressionAsText :: Expression -> Text
expressionAsText (Constant i) = show i
expressionAsText (Name expName) = expName
expressionAsText (Call callName params) = callName|+"("+|", " `intercalate` (expressionAsText <$> params)|+")"

eurmAsText :: EURM -> Text
eurmAsText (RawDeclaration decName instructions) =
  decName|+":\n"+|(unlines . toList $ ((\i -> "    " +|i|+"") . urmAsText <$> instructions))|+""

eurmAsText (AliasDeclaration decName decTarget) = decName|+" = "+|decTarget|+""

eurmAsText (CompositeDeclaration decName decParams decBody) = 
  decName|+"("+|", " `intercalate` decParams|+") = "+|expressionAsText decBody|+""

eurmAsText (RecursiveDeclaration decName decNonRecVars decRecVar decBaseCase decRecStep) =
  let firstPart = (decName|+"("+|", " `intercalate` decNonRecVars|++|unlessF (null decNonRecVars) ", "|+"") :: Text
  in (  firstPart|+"0) = "+|expressionAsText decBaseCase|+"\n"
     +| firstPart|++|decRecVar|+") = "+|expressionAsText decRecStep|+"")

eurmAsText bounded =
  let decName   = bounded ^. name
      decParams = ", " `intercalate` (bounded ^.. parameters.folded)
      decTag    = case getEURMTag bounded of
                    BoundedMinimizationDeclarationTag -> "μ"
                    BoundedSumDeclarationTag          -> "Σ"
                    BoundedProductDeclarationTag      -> "Π"
                    _ -> "" :: Text
      decIdx    = unwords $ bounded ^.. index
      decTop    = fromMaybe "" $ bounded ^? top.to expressionAsText
      decBody   = fromMaybe "" $ bounded ^? body.to expressionAsText
  in decName|+"("+|decParams|+") = "+|decTag|++|decIdx|+" < "+|decTop|+" ["+|decBody|+"]"
