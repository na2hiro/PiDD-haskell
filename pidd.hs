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


main = do
  let trs=map var2trans $ take 20 [0..]
  sequence_ $ map print trs
  sequence_ $ map (print . trans2var) trs


