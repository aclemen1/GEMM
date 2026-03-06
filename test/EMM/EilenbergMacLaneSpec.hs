module EMM.EilenbergMacLaneSpec (spec) where

import Test.Hspec
import EMM.Types
import EMM.EilenbergMacLane

spec :: Spec
spec = do
  describe "K(Z/2, 1) = RP^infty" $ do
    let h = emHomology 1 1 10

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` singletonGroup 0 1

    it "H_1 = Z/2" $
      groupInDegree 1 h `shouldBe` singletonGroup 1 1

    it "H_2 = 0 (integral homology of RP^infty)" $
      groupInDegree 2 h `shouldBe` emptyGroup

    it "H_3 = Z/2" $
      groupInDegree 3 h `shouldBe` singletonGroup 1 1

  describe "K(Z/2, 2)" $ do
    let h = emHomology 1 2 10

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` singletonGroup 0 1

    it "H_1 = 0" $
      groupInDegree 1 h `shouldBe` emptyGroup

    it "H_2 = Z/2" $
      groupInDegree 2 h `shouldBe` singletonGroup 1 1
