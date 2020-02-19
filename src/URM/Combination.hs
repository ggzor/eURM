module URM.Combination where

import URM.Core

import Control.Lens
import Data.Sequence as S  hiding (length, zip)
import qualified Data.List as L
import qualified Data.Foldable as F
import qualified Data.Vector as V

standarize :: Instructions -> Instructions
standarize program = program & traversed._Jump._3 %~ min (V.length program + 1)

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
compose :: Foldable f => Int -> f URM -> [Instructions] -> Instructions
compose n f gs =
  let k = length gs
      m = maximum ([n, k, ro f] ++ (ro <$> gs))
  in join $
          [ Block  $  uncurry  Transfer <$> zip [1..n] [m + 1..m + n] ]
       ++ ( Block  .  uncurry (call [m + 1..m + n]) <$> zip [m + n + 1..m + n + k] gs )
       ++ [ Block  $           call [m + n + 1..m + n + k] 1 f ]

recurse :: Int -> Instructions -> Instructions -> Instructions
recurse n baseCase recursiveStep =
  let m = maximum [n + 2, ro baseCase, ro recursiveStep]
      firstPart = joinSeq 
                  [ Block $ uncurry Transfer <$> zip [1..n + 1] [m + 1..m + n + 1]
                  , Block $         call [1..n] (m + n + 3) baseCase ]
      stopCondition =               Jump (m + n + 1) (m + n + 2) (F.length firstPart + 1 + F.length midPart + F.length lastPart)
      midPart =                     call ([m + 1..m + n] ++ [m + n + 2, m + n + 3]) (m + n + 3) recursiveStep
      lastPart =                  [ Successor (m + n + 2)
                                  , Jump 1 1 (F.length firstPart + 1)
                                  , Transfer (m + n + 3) 1 ]
  in seqToVector $ L.foldl' (:|>) (joinSeq [Block (firstPart :|> stopCondition), Block midPart]) lastPart

type InstructionsSeq = Seq URM
data Block = forall f. Foldable f => Block !(f URM)


seqToVector seq = V.fromListN (F.length seq) $ F.toList seq

join :: [Block] -> Instructions
join = seqToVector . joinSeq

joinSeq :: [Block] -> InstructionsSeq
joinSeq blocks =
  let append (result, size) (Block code) =
        let (newResult, len) = F.foldl' (\(result, len) inst -> let fixedInst = inst & _Jump._3 +~ size 
                                                                in (result :|> fixedInst, len + 1)) 
                                        (result, 0) code
        in (newResult, size + len)
      allInstructions = fst $ L.foldl' append (S.empty, 0) blocks
  in allInstructions
