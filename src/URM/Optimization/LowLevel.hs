module URM.Optimization.LowLevel where

import URM.Core

import Control.Applicative
import Control.Lens
import Control.Lens.Unsound
import Data.Foldable as F
import Data.Map.Strict as M
import Data.Maybe
import Data.Sequence as S
import Data.Vector as V

-- Searches for same register transfers and fixes backward and forward references in two passes
-- Complexity: O(n log n)
removeUselessTransfers :: Instructions -> Instructions
removeUselessTransfers code =
  let (backwardDestinations, updates) =
        let visitInstruction acc@(back, forward, updates, deleted) idx instruction =
              let newAcc = case instruction of
                             Jump _ _ target ->
                               if idx <= target
                                 then acc & _2 %~ appendFlippedReference
                                 else acc & _1 %~ appendFlippedReference
                                   where appendFlippedReference = M.alter (\curr -> ((idx :) <$> curr) <|> Just [idx]) (target - 1)
                             Transfer x y | x == y -> acc & _4 +~ 1
                             _ -> acc
                  updateForwardUpdates = maybe id (foldMap (flip M.insert (idx - deleted))) $ M.lookup idx forward
              in newAcc & _3 %~ updateForwardUpdates
        in V.ifoldl visitInstruction (M.empty, M.empty, M.empty, 0) code ^. lensProduct _1 _3
      modifyOrIgnoreInstruction acc@(result, backward, updates, deleted) idx instruction =
        let backwardTargets = M.fromList . fmap (, idx - deleted) <$> M.lookup idx backward
            newUpdates = maybe updates (M.union updates) backwardTargets
            newAcc = 
              case instruction of
                Transfer x y | x == y -> acc & _4 %~ (+1)
                Jump x y i ->
                  let updatedDestination = updates ^. at idx.non (idx - deleted) + 1
                  in acc & _1 %~ (:|> Jump x y updatedDestination)
                other -> acc & _1 %~ (:|> other)
            in newAcc & _3 .~ newUpdates
  in if V.null code
       then code
       else 
           V.fromList 
         . F.toList 
         .  view _1 
         $ V.ifoldl' modifyOrIgnoreInstruction (S.empty, backwardDestinations, updates, 0) code
