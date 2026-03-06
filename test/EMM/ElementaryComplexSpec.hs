module EMM.ElementaryComplexSpec (spec) where

import Test.Hspec
import EMM.Types
import EMM.ElementaryComplex

spec :: Spec
spec = do
  describe "nu2" $ do
    it "nu2(1) = 0" $ nu2 1 `shouldBe` 0
    it "nu2(2) = 1" $ nu2 2 `shouldBe` 1
    it "nu2(4) = 2" $ nu2 4 `shouldBe` 2
    it "nu2(6) = 1" $ nu2 6 `shouldBe` 1
    it "nu2(8) = 3" $ nu2 8 `shouldBe` 3

  describe "ecHomology even degree" $ do
    it "degree=2, logh=1, ac=6 has Z in degree 0" $
      groupInDegree 0 (ecHomology 2 1 6) `shouldBe` singletonGroup 0 1

    it "degree=2, logh=1, ac=6 has Z/2 in degree 2" $
      groupInDegree 2 (ecHomology 2 1 6) `shouldBe` singletonGroup 1 1

    it "degree=2, logh=1, ac=6 has Z/4 in degree 4" $
      groupInDegree 4 (ecHomology 2 1 6) `shouldBe` singletonGroup 2 1

    it "degree=2, logh=1, ac=6 has Z/2 in degree 6" $
      groupInDegree 6 (ecHomology 2 1 6) `shouldBe` singletonGroup 1 1

    it "degree=2, logh=1, ac=6 is trivial in odd degrees" $
      groupInDegree 3 (ecHomology 2 1 6) `shouldBe` emptyGroup

  describe "ecHomology odd degree" $ do
    it "degree=1, logh=1, ac=5 has Z/2 in degree 1" $
      groupInDegree 1 (ecHomology 1 1 5) `shouldBe` singletonGroup 1 1

    it "degree=1, logh=1, ac=5 has Z/2 in degree 3" $
      groupInDegree 3 (ecHomology 1 1 5) `shouldBe` singletonGroup 1 1

    it "degree=1, logh=1, ac=5 is trivial in degree 2" $
      groupInDegree 2 (ecHomology 1 1 5) `shouldBe` emptyGroup
