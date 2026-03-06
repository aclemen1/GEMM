module EMM.GradedGroupsSpec (spec) where

import Test.Hspec
import EMM.Types
import EMM.GradedGroups

spec :: Spec
spec = do
  describe "tensor" $ do
    it "Z tensor Z = Z" $
      tensor (0, 1) (0, 1) `shouldBe` (0, 1)

    it "Z tensor Z/2 = Z/2" $
      tensor (0, 1) (1, 1) `shouldBe` (1, 1)

    it "Z/2 tensor Z/4 = Z/2" $
      tensor (1, 1) (2, 1) `shouldBe` (1, 1)

    it "powers multiply" $
      tensor (1, 3) (2, 2) `shouldBe` (1, 6)

  describe "tor" $ do
    it "Tor(Z, anything) = 0" $
      tor (0, 1) (1, 5) `shouldBe` (0, 0)

    it "Tor(Z/2, Z/4) = Z/2" $
      tor (1, 1) (2, 1) `shouldBe` (1, 1)

  describe "hom" $ do
    it "Hom(Z, Z/2) = Z/2" $
      hom (0, 1) (1, 1) `shouldBe` (1, 1)

    it "Hom(Z/2, Z) has power 0" $
      snd (hom (1, 1) (0, 1)) `shouldBe` 0

  describe "ext" $ do
    it "Ext(Z, anything) has power 0" $
      snd (ext (0, 1) (1, 1)) `shouldBe` 0

    it "Ext(Z/2, Z) = Z/2" $
      ext (1, 1) (0, 1) `shouldBe` (1, 1)

  describe "universalCoefficients" $ do
    it "cohomology of trivial graded group is trivial" $
      groupInDegree 0 (universalCoefficients mempty) `shouldBe` emptyGroup
