import Test.Hspec
import PiDD
import Permutation

main = hspec spec

spec :: Spec
spec = do
    describe "example" $ do
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
