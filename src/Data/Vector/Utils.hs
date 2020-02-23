module Data.Vector.Utils where

import Data.Vector hiding (toList)
import Data.Foldable (toList)
import Data.Sequence hiding (fromList)

fromSeq :: Seq a -> Vector a
fromSeq = fromList . toList
