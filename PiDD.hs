module PiDD (
  Node(Empty,Base),node,
  fromseq,fromseqs,allseqs,dimN,calc,
  top,union,intsec,diff,dprod,cofact,papply,count
)where
import Node
import ZDD
import Permutation
-- PiDDの節

-- 節番号と互換の変換0:(1,0); 1:(2,1) 2:(2,0); 3:(3,2) 4:(3,1) 5:(3,0); 6:(4,3)...
var2trans :: Var -> Trans
var2trans v = chk 0 v
  where
    chk :: Int -> Int -> Trans
    chk x v
      | x*(x+1)<=2*v = chk (x+1) v
      | otherwise = let y=(x-1)-v+((x-1)*x `div` 2)
                    in Trans (x, y)

trans2var :: Trans -> Var
trans2var (Trans (x,y)) = (x-1)*x `div` 2 + (x-1) - y

-- PiDD
-- 接点ひとつ(0:左,1:右)
node_ :: Var -> Node -> Node -> Node
node_ v p Empty = p
node_ v p0 p1 = Node v p0 p1

showWithTrans :: Node -> String
showWithTrans (Node v n1 n2) = "(Node " ++ (show . getTrans . var2trans $ v) ++ " " ++ showWithTrans n1 ++ " " ++ showWithTrans n2 ++ ")"
showWithTrans Empty = "Empty"
showWithTrans Base = "Base"

-- Transから直接buildする
nodeT :: Trans -> Node -> Node -> Node
nodeT = node_ . trans2var

node :: (Int, Int) -> Node -> Node -> Node
node (x,y) | x==y = union
           | otherwise = nodeT. trans$ (x,y)



-- PiDD作成
fromseq :: Seq -> Node
fromseq = foldr papplyT Base . factors
fromseqs :: [Seq] -> Node
fromseqs = foldr (union . fromseq) Empty
-- (0...n)の順列を全て返す
allseqs :: Int -> Node
allseqs 0 = Base
allseqs n = let l = allseqs $ n-1
            in allseqs' l n 0
  where
    allseqs' :: Node -> Int -> Int -> Node
    allseqs' l x y
      | y==(x-1) = node (x,y) l l
      |otherwise = node (x,y) l $ allseqs' l x $ y+1



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



-- 直積
dprod :: Node -> Node -> Node
dprod _ Empty = Empty
dprod p Base  = p
dprod Empty _ = Empty
dprod Base q  = q
dprod p (Node v q0 q1) = (p `dprod` q0) `union` ((var2trans v) `papplyT` (p `dprod` q1))

-- cofact
cofact :: (Int,Int) -> Node -> Node
cofact _ Empty = Empty
cofact (x,y) Base
  | x==y = Base
  |otherwise = Empty
cofact pa@(x,y) n@(Node v p0 p1) =
  let tr = trans pa
      cv = trans2var tr
  in if v==cv then p1
     else 
      if x==y then cofact' x n
      else cofact' y (tr `papplyT` n)
  where
    -- cofact(v,v)
    cofact' :: Int -> Node -> Node
    cofact' _ Empty = Empty
    cofact' _ Base  = Base
    cofact' u (Node v p0 p1) =
      let Trans (x,y) = var2trans v
      in if x==u || y==u then cofact' u p0
         else node_ v (cofact' u p0) (cofact' u p1)

papply :: (Int,Int) -> Node -> Node
papply t@(x,y) n | x==y = n
                  | otherwise = papplyT (trans t) n

-- Seq集合にTransを適用する
papplyT :: Trans -> Node -> Node
papplyT t n
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
              node_ tv p1 p0
            else
              node (x,y') (papplyT t p0) (papply (u',v) p1)
