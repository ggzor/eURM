module Polysemy.Input.Utils where

import Polysemy
import Polysemy.Input
import Polysemy.State

inputToState :: Member (State s) r => Sem (Input s ': r) a -> Sem r a
inputToState = runInputSem get

mapInput :: Member (Input i2) r => (i2 -> i1) -> Sem (Input i1 ': r) a -> Sem r a
mapInput f = runInputSem $ f <$> input
