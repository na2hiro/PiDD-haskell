import Data.List
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
trans2var (Trans (x,y))
  | x<y = tr' y x
  |otherwise = tr' x y
  where
    tr' :: Int -> Int -> Var
    tr' x y = x*(x+1) `div` 2 + x - y

-- PiDD
-- 接点ひとつ(0:左,1:右)
data Node = Empty | Base | Node Var Node Node deriving(Show,Eq)
showWithTrans :: Node -> String
showWithTrans (Node v n1 n2) = "(Node " ++ (show . getTrans . var2trans $ v) ++ " " ++ showWithTrans n1 ++ " " ++ showWithTrans n2 ++ ")"
showWithTrans a = show a

-- Transから直接buildする
nodeT :: Trans -> Node -> Node -> Node
nodeT = Node . trans2var


-- PiDD作成
fromseq :: Seq -> Node
fromseq = foldl papply Base . factors

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
  | otherwise  = map foldseqr $ calc' n
  where
    calc' :: Node -> [[Trans]]
    calc' Empty = []
    calc' Base  = [[]]
    calc' (Node v p0 p1) = let tr=var2trans v
                           in (calc' p0) ++ (map (\trs -> tr:trs) $ calc' p1)

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

-- Seq集合にTransを適用する
papply :: Node -> Trans -> Node
papply n t = papply' n $ normalize t
  where
    papply' :: Node -> Trans -> Node
    papply' n t
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
             in if u'==x && v==y' then
                  -- 逆置換だった...いれかえる
                  Node v p1 p0
                else
                  nodeT (Trans (x,y')) (papply' p0 t) (papply' p1 $ Trans (u',v))



      

printT :: Node -> IO ()
printT = print . showWithTrans
{-
main = do
  let trs=map var2trans $ take 20 [0..]
  sequence_ $ map print trs
  sequence_ $ map (print . trans2var) trs
-}
--main = print $ showWithTrans $ fromseq $ Seq [1,0,2]
main = do
    printT $ papply (fromseq $ Seq [1,0,2]) $ Trans (2,0)
    printT $ papply (fromseq $ Seq [1,0,2]) $ Trans (1,0)
    printT $ (fromseq $ Seq [0,1,2]) `papply` Trans (2,1) `papply` Trans (1,0)
    let p= papply (fromseq $ Seq [1,0,2]) $ Trans (2,0)
    printT p
    print $ calc p


