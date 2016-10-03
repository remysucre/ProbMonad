module Gamble where

data Die = Die Int
  deriving (Eq, Show)
data Roll = Face Int
  deriving (Eq, Show)
data Flip = H | T
  deriving (Eq, Show)
