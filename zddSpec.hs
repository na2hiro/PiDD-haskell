import Test.Hspec
import ZDD

main = hspec spec

spec :: Spec
spec = do
    describe "example" $ do
      let a = Empty
      it "a=Empty" $ a `shouldBe` Empty
      let b = Base
      it "b=Base" $ b `shouldBe` Base
      let c = change b 1
      it "c=change b 1" $ c `shouldBe` getNode 1 Empty Base
      let d = change b 2
      it "d=change b 2" $ d `shouldBe` getNode 2 Empty Base
      let e = union c d
      it "e=union c d" $ e `shouldBe` getNode 2 (getNode 1 Empty Base) Base
      let f = union b e
      it "f=union b e" $ f `shouldBe` getNode 2 (getNode 1 Base Base) Base
      let g = diff f c
      it "g=diff f c" $ g `shouldBe` getNode 2 Base Base
