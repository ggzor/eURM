module URM.Interpreter (
  RegistersState, 
  Instructions, 
  evaluate
  ) where

import URM.Core

import Control.Lens hiding (index)
import Control.Monad.Trans.State
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Vector ((!?))

type RegistersState = M.Map Int Int

data URMState = URMState { _index :: !Int, _steps :: !Int, _registers :: !RegistersState }
makeLenses ''URMState

evaluate :: Int -> Instructions -> RegistersState -> Maybe Int
evaluate maxSteps instructions = evalState execution . URMState 0 0
  where execution =
          do idx <- use index
             step <- use steps
             if step == maxSteps
              then return Nothing
              else
                case instructions !? idx of
                  Nothing -> Just <$> use (registers.at 1.non 0)
                  Just instruction -> 
                    case instruction of
                      Zero r         -> continueAfter $ (registers.at r  ?=) =<< pure 0
                      Successor r     -> continueAfter $ (registers.at r  ?=) =<< use (registers.at r .non 0.to (+ 1))
                      Transfer r1 r2 -> continueAfter $ (registers.at r2 ?=) =<< use (registers.at r1.non 0)
                      Jump l r i     -> do lv <- use $ registers.at l.non 0
                                           rv <- use $ registers.at r.non 0
                                           if lv == rv 
                                             then do index .= (i - 1)
                                                     steps += 1
                                                     execution
                                             else continueAfter $ pure ()
        continueAfter action = do action
                                  index += 1
                                  steps += 1
                                  execution
