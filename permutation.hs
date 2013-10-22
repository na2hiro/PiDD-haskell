type Var = Int
-- 順列
data Permutation = Permu [Var] | Trans (Var,Var) deriving(Show)
-- 互換
instance Eq Permutation where
  Permu a == Permu b = a==b
  Trans a@(x1,y1) == Trans b@(x2,y2) = a==b || x1==y2 && x2==y1
  _ == _ = False

-- 恒等順列
pie :: Int -> Permutation
pie num = Permu [0..num]

-- 十分な長さの高等順列
piem :: Permutation -> Permutation
piem (Permu t) = pie $ length t
piem (Trans (x,y)) = pie $ max x y
-- 積
prod :: Permutation -> Permutation -> Permutation
a@(Trans (x1,y1)) `prod` b@(Trans (x2,y2)) = let m = maximum [x1,y1,x2,y2]
                                             in (pie m) `prod` a `prod` b
p@(Permu xs) `prod` (Trans (x,y))
  | x>=y = p `prod` Trans (y,x)
  | x<y  = Permu $ chk xs (x,y) 0
    where
      chk :: [Var] -> (Var,Var) -> Int -> [Var]
      chk (v:vs) p@(x,y) i
              | i<x  = v:chk vs p (i+1)
              | x==i = (xs!!y):chk vs p (i+1)
              | i<y  = v:chk vs p (i+1)
              | i==y = (xs!!x):vs

t@(Trans pa) `prod` p@(Permu xs) = (piem t) `prod` t `prod` p

p1@(Permu xs) `prod` p2@(Permu ys) = foldl prod p1 $ factors p2

-- 順列を互換の積に分解
factors :: Permutation -> [Permutation]
factors t@(Trans p)=[t]
factors p@(Permu xs)=snd $ factors' (xs,[])
                  where
                    factors' :: ([Var],[Permutation]) -> ([Var],[Permutation])
                    factors' (vs,rs)
                      | vs==[] =(vs,rs)
                      |otherwise = let lastpos=length vs - 1
                                       (maxpos,l)=look lastpos vs 0
                                   in case maxpos==lastpos of True  ->factors' (init vs,rs)    --互換いらない
                                                              False ->factors' (set maxpos l (init vs),(Trans (lastpos,maxpos)):rs)
                        where
                          -- 最大値のある場所と一番最後のやつを返す
                          look :: Int -> [Var] -> Int -> (Int,Var)
                          look i (x:xs) maxpos=case x==i of True  -> (maxpos,if xs==[] then maxpos else last xs)
                                                            False -> look i xs (maxpos+1)   --次を探す
                          -- 配列のi番目をvに変える
                          set :: Int -> a -> [a] -> [a]
                          set 0 v (x:xs) = v:xs
                          set i v (x:xs) = x:set (i-1) v xs

-- 順列の次元
dim :: Permutation -> Int
dim (Permu p) = dim' p 0
        where
          dim' :: [Var] -> Int -> Int
          dim' [] _ = 0
          dim' (x:xs) i
            | x==i      = dim' xs (i+1)
            | otherwise = max i $ dim' xs (i+1)
dim tr@(Trans t) = dim $ (piem tr) `prod` tr
   
{-
main =do
       let e = pie 4
       print e
       print $ e `prod` Trans (0,2) `prod` Trans (2,1)
       print $ dim $ Permu [1,2,0,4,3]
       print $ factors $ Permu [1,2,0,4,3]
-}
