{-# LANGUAGE NoImplicitPrelude #-}
module URM.Simple.Core where

import Control.Lens 
import Fmt

import Protolude
import Data.Semigroup

data URM = Zero      { _register :: !Int }
         | Successor { _register :: !Int }
         | Transfer  { _source   :: !Int, _target :: !Int }
         | Jump      { _left     :: !Int, _right  :: !Int, destination :: !Int }

makeLenses ''URM
makePrisms ''URM

urmAsText :: URM -> Text
urmAsText (Zero r)       = "Z("+|r|+")"
urmAsText (Successor r)  = "S("+|r|+")"
urmAsText (Transfer s t) = "T("+|s|+", "+|t|+")"
urmAsText (Jump l r d)   = "J("+|l|+", "+|r|+", "+|d|+")"

ro :: Foldable f => f URM -> Int
ro program = getMax $ foldl (\m a -> m <> Max (chooseRegisters a)) (Max 0) program
  where chooseRegisters (Zero r) = r
        chooseRegisters (Successor r) = r
        chooseRegisters (Transfer s t) = max s t
        chooseRegisters (Jump l r _) = max l r
