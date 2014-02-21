import Test.Hspec
import PiDD

main = hspec spec

spec :: Spec
spec = do
    describe "Figure 7" $ do
      let v21 = (2,1)
      let v32 = (3,2)
      let a = Base
      it "a=Base" $ a `shouldBe` Base
      let b = papply (2,1) a
      it "b=a.tau(2,1)" $ b `shouldBe` node v21 Empty Base
      let c = papply (3,2) a
      it "c=a.tau(3,2)" $ c `shouldBe` node v32 Empty Base
      let d = union a b
      it "d=union a b" $ d `shouldBe` node v21 Base Base
      let e = union c d
      it "e=union c d" $ e `shouldBe` node v32 (node v21 Base Base) Base
      let f = dprod d e
      it "f=d*e" $ f `shouldBe` node v32 (node v21 Base Base) (node v21 Base Base)
      let g = diff f e
      it "g=diff f e" $ g `shouldBe` node v32 Empty (node v21 Empty Base)

    describe "Figure 9" $ do
      let a21 = node (2,1) Base Base
      let a32 = node (3,2) a21 a21
      let a = node (3,1) a32 a21
      let b54 = node (5,4) Base Base
      let b65 = node (6,5) b54 b54
      let b = node (6,4) b65 b54
      let prodab = dprod a b

      let p54 = node (5,4) a a
      let p65 = node (6,5) p54 p54
      let p = node (6,4) p65 p54

      it "a=Base" $ prodab `shouldBe` p
    describe "allseqs" $ do
      let s0 = allseqs 0
      let s1 = allseqs 1
      let s2 = allseqs 2
      let s4 = allseqs 4
      it "0" $ s0 `shouldBe` Base
      let v10 = (1,0)
      let v20 = (2,0)
      let v21 = (2,1)
      let n10 = node v10 Base Base
      it "1" $ s1 `shouldBe` n10
      it "2" $ s2 `shouldBe` node v20 (node v21 n10 n10) n10
      it "4" $ count s4 `shouldBe` 120
    describe "(0,0)" $ do
      it "e.tau(0,0)" $ papply (0,0) Base `shouldBe` Base
      it "node (0,0) Base Empty" $ node (0,0) Base Empty `shouldBe` Base
      it "node (0,0) Empty Base" $ node (0,0) Empty Base `shouldBe` Base
    describe "cofact(n,n)" $ do
      let s3 = allseqs 3
      let s4 = allseqs 4
      it "allseqs 3.cofact(0,0)" $ count (cofact (0,0) s3) `shouldBe` 6
      it "allseqs 4.cofact(0,0)" $ count (cofact (0,0) s4) `shouldBe` 24
