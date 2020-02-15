module URM.Optimization.LowLevel where

import URM.Core

import Control.Lens
import Data.Foldable as F
import Data.Sequence as S
import Data.Vector as V

removeUselessTransfers :: Instructions -> Instructions
removeUselessTransfers code = 
  let deleteIfNeeded (result, deleteCount) instruction = 
        case instruction of
          Transfer x y | x == y -> (result, deleteCount + 1)
          Jump x y i -> (result :|> Jump x y (i - deleteCount), deleteCount)
          other -> (result :|> other, deleteCount)
  in if V.null code
       then code 
       else V.fromList . F.toList . fst $ V.foldl' deleteIfNeeded (S.empty, 0) code
