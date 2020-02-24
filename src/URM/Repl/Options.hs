{-# LANGUAGE NoImplicitPrelude #-}
module URM.Repl.Options where

import Control.Lens

import Protolude

data InterfaceOptions = 
  InterfaceOptions { _prompt :: !Text }
makeLenses ''InterfaceOptions

data ExecutionOptions = 
  ExecutionOptions { _maxSteps :: !Int }
makeLenses ''ExecutionOptions

data ReplOptions = 
  ReplOptions { _interfaceOptions :: !InterfaceOptions
              , _executionOptions :: !ExecutionOptions }

makeLenses ''ReplOptions