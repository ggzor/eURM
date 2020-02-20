module URM.Optimization.LowLevel where

import URM.Core
import URM.Combination

import Control.Applicative
import Control.Lens
import Control.Lens.Unsound
import Control.Monad
import Data.Bifunctor (first)
import qualified Data.Foldable as F
import Data.Graph
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.String as String
import qualified Data.Vector as V

import GHC.IO.Unsafe

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
                  in acc & _1 %~ (S.:|> Jump x y updatedDestination)
                other -> acc & _1 %~ (S.:|> other)
            in newAcc & _3 .~ newUpdates
  in if V.null code
       then code
       else
           V.fromList
         . F.toList
         .  view _1
         $ V.ifoldl' modifyOrIgnoreInstruction (S.empty, backwardDestinations, updates, 0) code

type ChunkIndex = Int
type Chunk = Instructions
type Chunks = [Chunk]

data RegisterValue = RegisterValue Int
                   | SettedZero | OriginalZero
                   | RegisterSuccessor RegisterValue
                   deriving Show
makePrisms ''RegisterValue

-- Pre-evaluates chunks of programs that doesn't contain loop-outs nor loop-ins to the efective
-- result of the chunk
-- Complexity: O(n log n)
preEvaluateChunks :: Maybe Int -> Instructions -> Instructions
preEvaluateChunks arityHint input =
  let arityHints = arityHint : repeat Nothing
      chunks = chunkify input
      n = length chunks
      chunkStarts = L.scanl' (+) 0 (F.length <$> chunks)
      findChunk idx = fromMaybe n $ (chunkStarts ^@? folded.filtered (>= idx)) ^? _Just._1
      evaluatedChunks = uncurry evaluateChunk <$> zip arityHints chunks
      optimizedChunks = uncurry buildOptimizedCode <$> zip evaluatedChunks chunks
      optimizedChunkStarts = M.fromList $ zip [0..] (L.scanl' (+) 0 (F.length <$> optimizedChunks))
      chunkToIndex chunk = fromMaybe undefined $ M.lookup chunk optimizedChunkStarts
  in if V.null input
       then input
       else reassembleCode findChunk chunkToIndex optimizedChunks

chunkify :: Instructions -> Chunks
chunkify code =
  let inIndices = Set.fromList $ code ^.. folded._Jump._3.to (+ (-1))
      outIndices = Set.fromAscList $ fst <$> code ^@.. ifolded._Jump
      chooseChunk ((s, e) : cs) idx instruction =
        if Set.member idx inIndices || Set.member e outIndices
          then (idx, idx) : (s, e) : cs
          else (s, idx) : cs
  in flip (uncurry V.slice) code . (\(s, e) -> (s, e - s + 1)) <$> L.reverse (V.ifoldl' chooseChunk [(0, 0)] code)

-- FIXME: Is it a semigroup?
mix :: RegisterValue -> RegisterValue -> RegisterValue
mix _ (RegisterValue x)     = RegisterValue x
mix OriginalZero SettedZero = OriginalZero
mix _            SettedZero = SettedZero
mix _ OriginalZero          = OriginalZero
mix y (RegisterSuccessor x) = RegisterSuccessor x

evaluateChunk :: Maybe Int -> Chunk -> Map Int RegisterValue
evaluateChunk arityHint chunk =
  let m = ro chunk
      arityBasedRegisters = ((\n -> (RegisterValue <$> [1..n]) L.++ (OriginalZero <$ [n + 1..m])) <$> arityHint)
      nonArityBasedRegisters = RegisterValue <$> [1..m]
      initialRegisters = M.fromAscList . L.zip [1..m] $ fromMaybe nonArityBasedRegisters arityBasedRegisters
      runInstruction registers idx instruction =
        let f = case instruction of
                  Zero reg          -> M.adjust (`mix` SettedZero) reg
                  Successor reg     -> M.adjust RegisterSuccessor reg
                  Transfer src dest -> M.adjust (maybe undefined (flip mix) $ M.lookup src registers) dest
                  other             -> id
        in f registers
  in V.ifoldl' runInstruction initialRegisters chunk

effectiveRegister :: Map Int RegisterValue -> Int -> Int
effectiveRegister registers idx =
  M.lookup idx registers & maybe idx
    ( let findRegister (RegisterValue i) = i
          findRegister (RegisterSuccessor v) = findRegister v
          findRegister _ = idx
      in findRegister )

isConstant :: RegisterValue -> Bool
isConstant SettedZero = True
isConstant OriginalZero = True
isConstant (RegisterSuccessor r) = isConstant r
isConstant (RegisterValue _) = False

registerValueDepth :: RegisterValue -> Int
registerValueDepth (RegisterSuccessor v) = 1 + registerValueDepth v
registerValueDepth _ = 0

buildOptimizedCode :: Map Int RegisterValue -> Chunk -> Chunk
buildOptimizedCode registers code =
  let firstJump = maybe [] ((: []) . review _Jump) (code ^? ix 0._Jump)
      lastJump = maybe [] ((: []) . review _Jump) (code ^? ix (F.length code - 1)._Jump)
      m = ro code
      writtenAndReadRegisters = Set.toAscList $ Set.intersection 
                                                  (Set.fromList $ code ^.. folded._Transfer._1) 
                                                  (Set.fromList $ code ^.. folded._Transfer._2)
      savedWRRegisters = L.zip writtenAndReadRegisters [m + 1..]
      transfersSavingWR = uncurry Transfer <$> savedWRRegisters
      originalWRRegistersMapping = M.fromAscList savedWRRegisters
      readRegister idx = fromMaybe idx $ M.lookup idx originalWRRegistersMapping
      requiredTransfers = L.filter (uncurry (/=)) (first (effectiveRegister registers) . (\x -> (x, x)) <$> [1..m])
      oldTransfers = uncurry Transfer . first readRegister <$> requiredTransfers

      transferGroups = L.foldl' (\m (s, d) -> M.alter (\l -> ((d :) <$> l) <|> Just [d]) s m) M.empty requiredTransfers
      graph = buildG (1, m) requiredTransfers
      transferOrder = reverse $ topSort graph
      transfers = uncurry Transfer <$> (transferOrder >>= (\s -> maybe [] (fmap (s, )) (M.lookup s transferGroups)))
      successors = registers ^@.. folded.filtered (has _RegisterSuccessor).to (\x -> (x, registerValueDepth x))
                    >>= (\(idx, (reg, times)) -> (guard (isConstant reg) >> [Zero (idx + 1)])  ++ (Successor (idx + 1) <$ replicate times ()))
      zeroes = Zero . (+ 1) . fst <$> registers ^@.. folded.filtered (has _SettedZero)
  in if F.length code <= 1
       then code
       else V.fromList $ firstJump ++ transfers ++ successors ++ zeroes ++ lastJump

reassembleCode :: (Int -> Int) -> (Int -> Int) -> Chunks -> Instructions
reassembleCode findChunk chunkToStart chunks =
  let traverseChunk = V.foldl' traverseInstruction
        where traverseInstruction result (Jump l r i) = result S.:|> Jump l r ((+ 1) . chunkToStart . findChunk $ i - 1)
              traverseInstruction result other = result S.:|> other
  in seqToVector $ L.foldl' traverseChunk S.empty chunks

-- seqToVector :: S.Seq a -> V.Vector a
-- seqToVector seq = V.fromListN (F.length seq) $ F.toList seq

prettyPrintRegisterValues values =
  let printValue :: (Int, RegisterValue) -> Maybe String
      printValue (idx, value) =
        let stringValue =
              case value of
                RegisterValue idx2 -> guard (idx /= idx2) >> Just ("[" ++ show idx2 ++ "]")
                SettedZero -> Just "0"
                OriginalZero -> Nothing
                RegisterSuccessor r -> Just $ "[" ++ show (effectiveRegister values idx) ++ "] + " ++ show (registerValueDepth value)
        in ((show idx ++ ": ") ++) <$> stringValue
  in "{ " ++ (String.unwords . L.intersperse "," . catMaybes $ (printValue <$> M.toAscList values)) ++ " }"
