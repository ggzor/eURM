module URM.Extended.BuiltIns where

import URM.Simple.Core

import Data.Vector

constant :: Int -> Vector URM
constant c = fromList $ Zero 1 : (Successor 1 <$ [1..c])

project :: Int -> Vector URM
project i = singleton $ Transfer i 1

successor :: Vector URM
successor = singleton $ Successor 1

signum :: Vector URM
signum = fromList [ Jump 1 2 4
                  , Successor 2
                  , Transfer 2 1 ]

sum :: Vector URM
sum = fromList [ Jump 2 3 5
               , Successor 1
               , Successor 3
               , Jump 1 1 1]

product :: Vector URM
product = fromList [ Jump 1 3 10
                   , Jump 2 3 10
                   , Zero 4
                   , Jump 1 4 8
                   , Successor 4
                   , Successor 5
                   , Jump 1 1 4
                   , Successor 3
                   , Jump 1 1 2
                   , Transfer 5 1 ]
