import Test.Hspec
import PiDD
import Permutation

main = hspec spec

spec :: Spec
spec = do
    describe "Figure 7" $ do
      let v21 = trans2var$ Trans (2,1)
      let v32 = trans2var$ Trans (3,2)
      let a = Base
      it "a=Base" $ a `shouldBe` Base
      let b = papply a (Trans (2,1))
      it "b=a.tau(2,1)" $ b `shouldBe` Node v21 Empty Base
      let c = papply a (Trans (3,2))
      it "c=a.tau(3,2)" $ c `shouldBe` Node v32 Empty Base
      let d = union a b
      it "d=union a b" $ d `shouldBe` Node v21 Base Base
      let e = union c d
      it "e=union c d" $ e `shouldBe` Node v32 (Node v21 Base Base) Base
      let f = dprod d e
      it "f=d*e" $ f `shouldBe` Node v32 (Node v21 Base Base) (Node v21 Base Base)
      let g = diff f e
      it "g=diff f e" $ g `shouldBe` Node v32 Empty (Node v21 Empty Base)

    describe "Figure 9" $ do
      let v21 = trans2var$ Trans (2,1)
      let v31 = trans2var$ Trans (3,1)
      let v32 = trans2var$ Trans (3,2)
      let a21 = Node v21 Base Base
      let a32 = Node v32 a21 a21
      let a = Node v31 a32 a21
      let v54 = trans2var$ Trans (5,4)
      let v64 = trans2var$ Trans (6,4)
      let v65 = trans2var$ Trans (6,5)
      let b54 = Node v54 Base Base
      let b65 = Node v65 b54 b54
      let b = Node v64 b65 b54
      let prodab = dprod a b

      let p54 = Node v54 a a
      let p65 = Node v65 p54 p54
      let p = Node v64 p65 p54

      it "a=Base" $ prodab `shouldBe` p
