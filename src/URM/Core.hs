module URM.Core where

import Control.Lens
import Data.Vector (Vector)

type Instructions = Vector URM

data URM = Zero      { _register :: !Int }
         | Successor { _register :: !Int }
         | Transfer  { _source :: !Int, _target :: !Int }
         | Jump      { _left :: !Int, _right :: !Int, destination :: !Int }

makeLenses ''URM
makePrisms ''URM

instance Show URM where
  show (Zero r) = "Z(" ++ show r ++ ")"
  show (Successor r) = "S(" ++ show r ++ ")"
  show (Transfer s t) = "T(" ++ show s ++ ", " ++ show t ++ ")"
  show (Jump l r i) = "J(" ++ show l ++ ", " ++ show r ++ ", " ++ show i ++ ")"
