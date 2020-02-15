module URM.Core where

import Control.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

type Instructions = V.Vector URM

data URM = Zero      { _register :: !Int }
         | Successor { _register :: !Int }
         | Transfer  { _source :: !Int, _target :: !Int }
         | Jump      { _left :: !Int, _right :: !Int, destination :: !Int }

makeLenses ''URM
makePrisms ''URM

urmConstant :: Int -> Instructions
urmConstant i = V.fromList $ Zero 1 : (Successor 1 <$ [1..i])

urmProject :: Int -> Instructions
urmProject i = V.singleton $ Transfer i 1

-- FIXME: Optimizable with Max functor
ro :: Instructions -> Int
ro = fromMaybe 0 . maximumOf (folded.folding
        (\i -> foldMap (i ^..) [register, source, target, left, right]))

instance Show URM where
  show (Zero r) = "Z(" ++ show r ++ ")"
  show (Successor r) = "S(" ++ show r ++ ")"
  show (Transfer s t) = "T(" ++ show s ++ ", " ++ show t ++ ")"
  show (Jump l r i) = "J(" ++ show l ++ ", " ++ show r ++ ", " ++ show i ++ ")"
