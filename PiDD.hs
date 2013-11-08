module PiDD (
  Node(Empty,Base),node,
  var2trans,trans2var,showWithTrans,nodeT,
  fromseq,fromseqs,dimN,calc,
  top,union,intsec,diff,dprod,cofact,papply
)where
import Permutation
-- PiDDの節
type Var = Int

-- 節番号と互換の変換0:(0,0); 1:(1,1) 2:(1,0); 3:(2,2) 4:(2,1) 5:(2,0); 6:(3,3) 7:(3,2) 8:(3,1) 9:(3,0); 10:(4,4)...
var2trans :: Var -> Trans
var2trans v = chk 0 v
  where
    chk :: Int -> Int -> Trans
    chk x v
      | x*(x+1)<=2*v = chk (x+1) v
      | otherwise = let y=(x-1)-v+((x-1)*x `div` 2)
                    in Trans (x-1,y)

trans2var :: Trans -> Var
trans2var (Trans (x,y)) = x*(x+1) `div` 2 + x - y

-- PiDD
-- 接点ひとつ(0:左,1:右)
data Node = Empty | Base | Node Var Node Node deriving(Show,Eq)
node :: Var -> Node -> Node -> Node
node v p Empty = p
node v p0 p1 = Node v p0 p1

showWithTrans :: Node -> String
showWithTrans (Node v n1 n2) = "(Node " ++ (show . getTrans . var2trans $ v) ++ " " ++ showWithTrans n1 ++ " " ++ showWithTrans n2 ++ ")"
showWithTrans a = show a

-- Transから直接buildする
nodeT :: Trans -> Node -> Node -> Node
nodeT = node . trans2var


-- PiDD作成
fromseq :: Seq -> Node
fromseq = foldr (flip papply) Base . factors
fromseqs :: [Seq] -> Node
fromseqs = foldr (union . fromseq) Empty

-- PiDDであらわされる順列の最大
dimN :: Node -> Int
dimN Empty = 0
dimN Base  = 0
dimN (Node v p0 p1) = max ((dim . var2trans) v) $ max (dimN p0) (dimN p1)

-- PiDDから順列の集合を返す
calc :: Node -> [Seq]
calc n
  | n == Empty = []
  | n == Base  = [pie . dimN $ n]
  | otherwise  = map foldseql $ calc' n
  where
    calc' :: Node -> [[Trans]]
    calc' Empty = []
    calc' Base  = [[]]
    calc' (Node v p0 p1) = let tr=var2trans v
                           in (calc' p0) ++ (map (tr:) $ calc' p1)

-- PiDD操作
-- root接点を返す
top :: Node -> Var
top Empty = undefined
top Base  = undefined
top (Node v _ _) = v
-- transで返す
topT :: Node -> Trans
topT = var2trans . top
-- 順列の最大次元を返す

-- 2つのPiDDの和集合
union :: Node -> Node -> Node
union Empty q = q
union Base Empty = Base
union Base Base  = Base
union Base (Node v p0 p1) = node v (Base `union` p0) p1
union p Empty = p
union (Node v p0 p1) Base  =node v (Base `union` p0) p1
union p@(Node v1 p0 p1) q@(Node v2 q0 q1)
  | v1<v2 = node v2 (q0 `union` p) q1
  | v1>v2 = node v1 (p0 `union` q) p1
  |otherwise= node v1 (p0 `union` q0) (p1 `union` q1)

-- 2つのPiDDの積集合
intsec :: Node -> Node -> Node
intsec Empty _ = Empty
intsec Base Empty = Empty
intsec Base Base  = Base
intsec Base (Node v p0 p1) = intsec Base p0
intsec _ Empty = Empty
intsec (Node v p0 p1) Base = intsec Base p0
intsec p@(Node v1 p0 p1) q@(Node v2 q0 q1)
  | v1<v2 = p `intsec` q0
  | v1>v2 = p0 `intsec` q
  | v1==v2= node v1 (p0 `intsec` q0) (p1 `intsec` q1)

-- 2つのPiDDの差集合
diff :: Node -> Node -> Node
diff Empty _ = Empty
diff p Empty = p
diff Base Base = Empty
diff Base (Node v p0 p1) = diff Base p0
diff (Node v p0 p1) Base = node v (diff p0 Base) p1
diff p@(Node v1 p0 p1) q@(Node v2 q0 q1)
  | v1<v2 = diff p q0
  | v1>v2 = node v1 (diff p0 q) p1
  | v1==v2= node v1 (diff p0 q0) (diff p1 q1)

-- 直積
dprod :: Node -> Node -> Node
dprod _ Empty = Empty
dprod p Base  = p
dprod Empty _ = Empty
dprod Base q  = q
dprod p (Node v q0 q1) = (p `dprod` q0) `union` ((p `dprod` q1) `papply` (var2trans v))

-- cofact
cofact :: Node -> (Int,Int) -> Node
cofact Empty _ = Empty
cofact Base (x,y)
  | x==y = Base
  |otherwise = Empty
cofact n@(Node v p0 p1) pa@(x,y) =
  let tr = trans pa
      cv = trans2var tr
  in if v==cv then p1
     else 
      if x==y then cofact' n x
      else cofact' (n `papply` tr) y
  where
    -- cofact(v,v)
    cofact' :: Node -> Int -> Node
    cofact' Empty _ = Empty
    cofact' Base  _ = Base
    cofact' (Node v p0 p1) u =
      let Trans (x,y) = var2trans v
      in if x==u || y==u then cofact' p0 u
         else node v (cofact' p0 u) (cofact' p1 u)


-- Seq集合にTransを適用する
papply :: Node -> Trans -> Node
papply n t
  | n == Empty = Empty
  | n == Base  = nodeT t Empty n
  | otherwise  =
    let Node tv p0 p1 = n
        Trans (x,y) = var2trans tv
        Trans (u,v) = t
    in if u>x then
          nodeT t Empty n
       else
         -- 互換の積(x,y) . (u,v) を (u',v) . (x,y') の形に変換できる (しかもu'<xでx>y')
         let (u',y') = if y==u then (u,v)
                       else if y==v then (u,u)
                       else if x==u then (y,y)
                       else (u,y)
         in if u==x && v==y then
              -- 逆置換だった...いれかえる
              node tv p1 p0
            else
              nodeT (Trans (x,y')) (papply p0 t) (papply p1 $ Trans (u',v))

printT :: Node -> IO ()
printT = print . showWithTrans
{-
main = do
  let trs=map var2trans $ take 20 [0..]
  sequence_ $ map print trs
  sequence_ $ map (print . trans2var) trs
-}
--main = print $ showWithTrans $ fromseq $ Seq [1,0,2]
{-
main = do
    printT $ papply (fromseq $ Seq [1,0,2]) $ Trans (2,0)
    printT $ papply (fromseq $ Seq [1,0,2]) $ Trans (1,0)
    printT $ (fromseq $ Seq [0,1,2]) `papply` Trans (2,1) `papply` Trans (1,0)
    let p= papply (fromseq $ Seq [1,0,2]) $ Trans (2,0)
    printT p
    print $ calc p
    print . calc $ nodeT (Trans (2,0)) (nodeT (Trans (1,0)) Empty Base) Base
    -}
{-
main = do
    let p = fromseq $ Seq [0,2,1]
    let q = fromseq $ Seq [2,1,0]
    printT p
    printT q
    let r = p `union` q
    printT r
    print .calc $ r
-}
{-
main = do
    let p = fromseq $ Seq [0,2,1]
        q = fromseq $ Seq [2,1,0]
        r = p `union` q
    printT r
    let s = r `intsec` p
    printT s
    print .calc $ s
    print .calc $ r `diff` p
-}
{-
main = do
    let p = fromseqs $ [Seq [2,1,0],Seq [1,2,0],Seq [0,2,1],Seq [1,0]]
    printT p
    print .calc $ p
    let q = cofact p (2,0)
    printT q
    print .calc $ q
-}
{-
main = do
    let p = fromseqs $ [Seq [0,1,2],Seq [1,0,2]]
        q = fromseqs $ [Seq [0,2,1],Seq [1,2,0]]
        r = dprod p q
    printT r
    print.calc$r
-}
{-
printS :: Node -> IO ()
printS = print . calc
main = do
    let a = Base
        b = a `papply` Trans (1,0)
        c = a `papply` Trans (2,1)
        d = a `union` b
        e = c `union` d
        f = e `dprod` d
        g = f `diff` e
    sequence_ $ map printS [a,b,c,d,e,f,g]

-}


