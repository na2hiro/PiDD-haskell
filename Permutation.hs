module Permutation (
  Seq(Seq), Trans(Trans),
  getSeq, getTrans,
  size, dim, factors,
  pie, piem, normalize, prod, apply, foldseql, foldseqr
) where

-- 順列
newtype Seq = Seq { getSeq :: [Int] } deriving(Show,Eq)
-- 互換
newtype Trans = Trans { getTrans :: (Int,Int) } deriving(Show)
instance Eq Trans where
  Trans a@(x1,y1) == Trans b@(x2,y2) = a==b || x1==y2 && x2==y1

instance Ord Trans where
  Trans (x1,y1) `compare` Trans (x2,y2)
    | x1 < y1 = LT
    | x1 > y1 = GT
    | x1==y1  = y2 `compare` y1 --ここが逆なので注意

-- 置換
class Permutation a where
  --表すのに必要な大きさ
  size :: a -> Int
  --次元
  dim :: a -> Int
  --互換の積に分解。小さいやつが先にくる
  factors :: a -> [Trans]

instance Permutation Seq where
  size = length  . getSeq
  dim (Seq s) = dim' s 0
        where
          dim' :: [Int] -> Int -> Int
          dim' [] _ = 0
          dim' (x:xs) i
            | x==i      = dim' xs (i+1)
            | otherwise = max i $ dim' xs (i+1)

  factors p@(Seq xs)=snd $ factors' (xs,[])
                  where
                    factors' :: ([Int],[Trans]) -> ([Int],[Trans])
                    factors' (vs,rs)
                      | vs==[] =(vs,rs)
                      |otherwise = let lastpos=length vs - 1
                                       (maxpos,l)=look lastpos vs 0
                                   in case maxpos==lastpos of True  ->factors' (init vs,rs)    --互換いらない
                                                              False ->factors' (set maxpos l (init vs),(Trans (lastpos,maxpos)):rs)
                        where
                          -- 最大値のある場所と一番最後のやつを返す
                          look :: Int -> [Int] -> Int -> (Int,Int)
                          look i (x:xs) maxpos=case x==i of True  -> (maxpos,if xs==[] then maxpos else last xs)
                                                            False -> look i xs (maxpos+1)   --次を探す
                          -- 配列のi番目をvに変える
                          set :: Int -> a -> [a] -> [a]
                          set 0 v (x:xs) = v:xs
                          set i v (x:xs) = x:set (i-1) v xs

instance Permutation Trans where
  size (Trans (x,y)) = max x y
  dim = size
  factors = return

-- 恒等順列(d:次元)
pie :: Int -> Seq
pie d = Seq $ take (1+d) [0..]

-- 十分な長さの高等順列
piem :: (Permutation a)=> a -> Seq
piem t = pie $ size t

-- 互換の正規化（大きい方を前に）
normalize :: Trans -> Trans
normalize t@(Trans (x,y))
  | x<y = Trans (y,x)
  |otherwise = t

-- 積
prod :: (Permutation a)=> Seq -> a -> Seq
prod p q = foldl apply p $ factors q

-- 互換を1つ適用
apply :: Seq -> Trans -> Seq
p@(Seq xs) `apply` (Trans (x,y))
  | x>=y = p `apply` Trans (y,x)
  | x<y  = Seq $ chk xs (x,y) 0
    where
      chk :: [Int] -> (Int,Int) -> Int -> [Int]
      chk (v:vs) p@(x,y) i
              | i<x  = v:chk vs p (i+1)
              | x==i = (xs!!y):chk vs p (i+1)
              | i<y  = v:chk vs p (i+1)
              | i==y = (xs!!x):vs

-- 互換の列の最大次元
dimmax :: [Trans] -> Int
dimmax = foldr max' 0
  where max' (Trans (x,y)) m = max m $ max x y
-- 互換の列から順列を得る（左から順に適用と右から順に適用がある）
foldseql :: [Trans] -> Seq
foldseql ts= foldl apply (pie . dimmax $ ts) ts
foldseqr :: [Trans] -> Seq
foldseqr ts= foldr (flip apply) (pie . dimmax $ ts) ts
