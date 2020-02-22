module URM.Extended.Combination where

import URM.Simple.Core

import Control.Lens

import Data.Sequence hiding (length, zip)
import Data.List as L

type InstructionsSeq = Seq URM

standarize :: forall f. Traversable f => f URM -> f URM
standarize program = 
  program & traversed._Jump._3 %~ min (length program + 1)

data Block = forall f. Foldable f => Block !(f URM)

joinSeq :: [Block] -> InstructionsSeq
joinSeq blocks =
  let append (result, size) (Block code) =
        let (newResult, len) = 
              foldl' (\(acc, curLen) inst -> let fixedInst = inst & _Jump._3 +~ size 
                                             in (acc :|> fixedInst, curLen + 1)) (result, 0) code
        in (newResult, size + len)
      allInstructions = fst $ foldl' append (empty, 0) blocks
  in allInstructions

-- Assumes program is standarized
call :: Foldable f => [Int] -> Int -> f URM -> InstructionsSeq
call parameters result program = 
  let n = length parameters
  in joinSeq
       [ Block $ uncurry Transfer <$> zip parameters [1..n]
       , Block $         Zero <$> [n + 1..ro program]
       , Block           program
       , Block          [Transfer 1 result]]

-- Assumes f and gs are standarized
compose :: (Foldable f, Foldable g) => Int -> f URM -> [g URM] -> InstructionsSeq
compose n f gs =
  let k = length gs
      m = maximum ([n, k, ro f] ++ (ro <$> gs))
  in joinSeq $
          [ Block  $  uncurry  Transfer <$> zip [1..n] [m + 1..m + n] ]
       ++ ( Block  .  uncurry (call [m + 1..m + n]) <$> zip [m + n + 1..m + n + k] gs )
       ++ [ Block  $           call [m + n + 1..m + n + k] 1 f ]

-- Assumes baseCase and recursiveStep are standarized
recurse :: (Foldable f, Foldable g) => Int -> f URM -> g URM -> InstructionsSeq
recurse n baseCase recursiveStep =
  let m = maximum [n + 2, ro baseCase, ro recursiveStep]
      firstPart = joinSeq 
                  [ Block $ uncurry Transfer <$> zip [1..n + 1] [m + 1..m + n + 1]
                  , Block $         call [1..n] (m + n + 3) baseCase ]
      stopCondition =               Jump (m + n + 1) (m + n + 2) (length firstPart + 1 + length midPart + length lastPart)
      midPart =                     call ([m + 1..m + n] ++ [m + n + 2, m + n + 3]) (m + n + 3) recursiveStep
      lastPart =                  [ Successor (m + n + 2)
                                  , Jump 1 1 (length firstPart + 1)
                                  , Transfer (m + n + 3) 1 ]
  in foldl' (:|>) (joinSeq [Block (firstPart :|> stopCondition), Block midPart]) lastPart
