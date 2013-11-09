module Node where

data Node = Node Var Node Node Count
          | Empty
          | Base deriving(Show, Eq)

type Var = Int
type Count = Int

count :: Node->Int
count Empty = 0
count Base = 1
count (Node _ _ _ c) = c
