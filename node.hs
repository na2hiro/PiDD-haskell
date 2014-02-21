module Node where

data Node = Node Var Node Node Count
          | Empty
          | Base deriving(Show, Eq)

type Var = Int
type Count = Integer

count :: Node->Count
count Empty = 0
count Base = 1
count (Node _ _ _ c) = c

toEdges :: Node->[(String,String,Bool)]
toEdges n = f Nothing True n
  where f :: (Maybe String)->Bool->Node->[(String,String,Bool)]
        f x b nod | Empty<-nod = this
                  | Base<-nod = this
                  | Node v n1 n2 c<-nod =  this++f (Just$toStr nod) False n1++f (Just$ toStr nod) True n2
          where this = maybe [] (\xx->[(xx,toStr nod,b)]) x
        toStr Empty = "Empty"
        toStr Base = "Base"
        toStr (Node v _ _ _) = "var"++show v

toDot :: Node->String
toDot n = "digraph sample{\n"++(concat. map (\(a,b,tf)->a++"->"++b++(if tf then "" else "[style=dotted]")++";\n"). toEdges$ n)++"}"
