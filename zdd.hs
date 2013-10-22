data Node = Node Var Node Node
          | Empty
          | Base deriving(Show, Eq)

type Var = Int

getNode :: Var->Node->Node->Node
getNode top p0 Empty = p0
getNode top p0 p1 = Node top p0 p1

subset1 :: Node->Var->Node
subset1 (Node top p0 p1) var
  | top<var = Empty
  | top==var = p1
  | top>var = getNode top (subset1 p0 var) (subset1 p1 var)

subset0 :: Node->Var->Node
subset0 p@(Node top p0 p1) var
  | top<var = p
  | top==var = p0
  | top>var = getNode top (subset0 p0 var) (subset0 p1 var)

change :: Node->Var->Node
change p@(Node top p0 p1) var
  | top<var = getNode var Empty p
  | top==var = getNode var p1 p0
  | top>var = getNode top (change p0 var) (change p1 var)

union :: Node->Node->Node
union Empty q = q
union p Empty = p
union p@(Node ptop p0 p1) q@(Node qtop q0 q1)
  | p==q = p
  | ptop>qtop = getNode ptop (union p0 q) p1
  | ptop<qtop = getNode qtop (union p q0) q1
  | ptop==qtop = getNode ptop (union p0 q0) (union p1 q1)

intsec :: Node->Node->Node
intsec Empty _ = Empty
intsec _ Empty = Empty
intsec p@(Node ptop p0 p1) q@(Node qtop q0 q1)
  | p==q = p
  | ptop>qtop = intsec p0 q
  | ptop<qtop = intsec p q0
  | ptop==qtop = getNode ptop (intsec p0 q0) (intsec p1 q1)

diff :: Node->Node->Node
diff Empty _ = Empty
diff p Empty = p
diff p@(Node ptop p0 p1) q@(Node qtop q0 q1)
  | ptop>qtop = getNode ptop (diff p0 q) p1
  | ptop<qtop = diff p q0
  | ptop==qtop = getNode ptop (diff p0 q0) (diff p1 q1)

count :: Node->Int
count Empty = 0
count Base = 1
count (Node _ p0 p1) = count p0 + count p1

