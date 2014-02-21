module ZDD(
  Node(Base, Empty), getNode, subset1, subset0, change,
  union, intsec, diff, count, getTop, toDot
) where
import Node

-- 本来は(top, p0, p1)をキーとするノードテーブルを引くべき
getNode :: Var->Node->Node->Node
getNode _ p0 Empty = p0
getNode top p0 p1 = Node top p0 p1 (count p0+count p1)

-- 'a variable with highest order'
getTop :: Node->Var
getTop (Node v _ _ _) = v
getTop _ = minBound

subset1 :: Node->Var->Node
subset1 node var
  | top<var = Empty
  | top==var = p1
  | top>var = getNode top (subset1 p0 var) (subset1 p1 var)
  where top = getTop node
        Node _ p0 p1 _ = node

subset0 :: Node->Var->Node
subset0 p var
  | top<var = p
  | top==var = p0
  | top>var = getNode top (subset0 p0 var) (subset0 p1 var)
  where top = getTop p
        Node _ p0 p1 _ = p

change :: Node->Var->Node
change p var
  | top<var = getNode var Empty p
  | top==var = getNode var p1 p0
  | top>var = getNode top (change p0 var) (change p1 var)
  where top = getTop p
        Node _ p0 p1 _ = p

union :: Node->Node->Node
union Empty q = q
union p Empty = p
union p q
  | p==q = p -- 参照の一致で判定すべき
  | ptop>qtop = getNode ptop (union p0 q) p1
  | ptop<qtop = getNode qtop (union p q0) q1
  | ptop==qtop = getNode ptop (union p0 q0) (union p1 q1)
  where ptop = getTop p
        qtop = getTop q
        Node _ p0 p1 _ = p
        Node _ q0 q1 _ = q

intsec :: Node->Node->Node
intsec Empty _ = Empty
intsec _ Empty = Empty
intsec p q
  | p==q = p -- 参照の一致で判定すべき
  | ptop>qtop = intsec p0 q
  | ptop<qtop = intsec p q0
  | ptop==qtop = getNode ptop (intsec p0 q0) (intsec p1 q1)
  where ptop = getTop p
        qtop = getTop q
        Node _ p0 p1 _ = p
        Node _ q0 q1 _ = q

diff :: Node->Node->Node
diff Empty _ = Empty
diff p Empty = p
diff p q
  | p==q = Empty -- 参照の一致で判定すべき
  | ptop>qtop = getNode ptop (diff p0 q) p1
  | ptop<qtop = diff p q0
  | ptop==qtop = getNode ptop (diff p0 q0) (diff p1 q1)
  where ptop = getTop p
        qtop = getTop q
        Node _ p0 p1 _ = p
        Node _ q0 q1 _ = q
