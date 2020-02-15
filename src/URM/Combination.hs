module URM.Combination where

import URM.Core

import Control.Lens
import qualified Data.Vector as V
import Prelude hiding (concat)

standarize :: Instructions -> Instructions
standarize program = program & traversed._Jump._3 %~ min (V.length program + 1)

-- Assumes top is standarized
concat :: Instructions -> Instructions -> Instructions
concat top bottom = top V.++ offsettedBottom
  where offsettedBottom = bottom & traversed._Jump._3 +~ V.length top

-- FIXME: Use proper vector functions
-- Assumes program is standarized
call :: [Int] -> Int -> Instructions -> Instructions
call parameters result program = 
  let n = length parameters
  in V.fromList 
       ((uncurry Transfer <$> zip parameters [1..n])
       ++       (Zero <$> [n + 1..ro program]))
     `concat`    program
     `V.snoc`    Transfer 1 result

-- Assumes f and gs are standarized
compose :: Int -> Instructions -> [Instructions] -> Instructions
compose n f gs =
  let k = length gs
      m = maximum ([n, k, ro f] ++ (ro <$> gs))
  in Prelude.foldl concat
       (V.fromList (uncurry Transfer <$> zip [1..n] [m + 1..m + n]))
       (uncurry            (call [m + 1..m + n]) <$> zip [m + n + 1..m + n + k] gs)
       `concat`             call [m + n + 1..m + n + k] 1 f

recurse :: Int -> Instructions -> Instructions -> Instructions
recurse n baseCase recursiveStep =
  let m = maximum [n + 2, ro baseCase, ro recursiveStep]
      firstPart = 
        V.fromList 
        (uncurry      Transfer <$> zip [1..n + 1] [m + 1..m + n + 1])
        `concat`      call [1..n] (m + n + 3) baseCase
      stopCondition = Jump (m + n + 1) (m + n + 2) (V.length firstPart + 1 + V.length lastPart)
      lastPart =      call ([m + 1..m + n] ++ [m + n + 2, m + n + 3]) (m + n + 3) recursiveStep
        `V.snoc`      Successor (m + n + 2)
        `V.snoc`      Jump 1 1 (V.length firstPart - V.length firstPart)
        `V.snoc`      Transfer (m + n + 3) 1
  in  firstPart `V.snoc` stopCondition `concat` lastPart
