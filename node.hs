module Node where

data Node = Node Var Node Node
          | Empty
          | Base deriving(Show, Eq)

type Var = Int
