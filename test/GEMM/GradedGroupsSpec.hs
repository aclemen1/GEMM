module GEMM.GradedGroupsSpec (spec) where

import Test.Hspec
import GEMM.Types
import GEMM.GradedGroups

spec :: Spec
spec = do
  describe "tensor" $ do
    it "Z tensor Z = Z" $
      tensor ((0,0), 1) ((0,0), 1) `shouldBe` ((0,0), 1)

    it "Z tensor Z/2 = Z/2" $
      tensor ((0,0), 1) ((2,1), 1) `shouldBe` ((2,1), 1)

    it "Z/2 tensor Z/4 = Z/2" $
      tensor ((2,1), 1) ((2,2), 1) `shouldBe` ((2,1), 1)

    it "powers multiply" $
      tensor ((2,1), 3) ((2,2), 2) `shouldBe` ((2,1), 6)

    it "different primes give 0" $
      tensor ((2,1), 1) ((3,1), 1) `shouldBe` ((0,0), 0)

  describe "tor" $ do
    it "Tor(Z, anything) = 0" $
      tor ((0,0), 1) ((2,1), 5) `shouldBe` ((0,0), 0)

    it "Tor(Z/2, Z/4) = Z/2" $
      tor ((2,1), 1) ((2,2), 1) `shouldBe` ((2,1), 1)

    it "Tor at different primes = 0" $
      tor ((2,1), 1) ((3,1), 1) `shouldBe` ((0,0), 0)

  describe "hom" $ do
    it "Hom(Z, Z/2) = Z/2" $
      hom ((0,0), 1) ((2,1), 1) `shouldBe` ((2,1), 1)

    it "Hom(Z/2, Z) = 0" $
      snd (hom ((2,1), 1) ((0,0), 1)) `shouldBe` 0

  describe "ext" $ do
    it "Ext(Z, anything) = 0" $
      snd (ext ((0,0), 1) ((2,1), 1)) `shouldBe` 0

    it "Ext(Z/2, Z) = Z/2" $
      ext ((2,1), 1) ((0,0), 1) `shouldBe` ((2,1), 1)

  describe "universalCoefficients" $ do
    it "cohomology of trivial graded group is trivial" $
      groupInDegree 0 (universalCoefficients mempty) `shouldBe` emptyGroup
