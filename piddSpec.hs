import Test.Hspec
import PiDD
import Permutation

main = hspec spec

spec :: Spec
spec = do
    describe "Figure 7" $ do
      let v21 = trans2var$ trans (2,1)
      let v32 = trans2var$ trans (3,2)
      let a = Base
      it "a=Base" $ a `shouldBe` Base
      let b = papply a (trans (2,1))
      it "b=a.tau(2,1)" $ b `shouldBe` node v21 Empty Base
      let c = papply a (trans (3,2))
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
      let v21 = trans2var$ trans (2,1)
      let v31 = trans2var$ trans (3,1)
      let v32 = trans2var$ trans (3,2)
      let a21 = node v21 Base Base
      let a32 = node v32 a21 a21
      let a = node v31 a32 a21
      let v54 = trans2var$ trans (5,4)
      let v64 = trans2var$ trans (6,4)
      let v65 = trans2var$ trans (6,5)
      let b54 = node v54 Base Base
      let b65 = node v65 b54 b54
      let b = node v64 b65 b54
      let prodab = dprod a b

      let p54 = node v54 a a
      let p65 = node v65 p54 p54
      let p = node v64 p65 p54

      it "a=Base" $ prodab `shouldBe` p
