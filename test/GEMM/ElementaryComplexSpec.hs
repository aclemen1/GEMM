module GEMM.ElementaryComplexSpec (spec) where

import Test.Hspec
import GEMM.Types
import GEMM.ElementaryComplex

spec :: Spec
spec = do
  describe "nuP" $ do
    it "nu2(1) = 0" $ nuP 2 1 `shouldBe` 0
    it "nu2(2) = 1" $ nuP 2 2 `shouldBe` 1
    it "nu2(4) = 2" $ nuP 2 4 `shouldBe` 2
    it "nu2(6) = 1" $ nuP 2 6 `shouldBe` 1
    it "nu2(8) = 3" $ nuP 2 8 `shouldBe` 3
    it "nu3(9) = 2" $ nuP 3 9 `shouldBe` 2
    it "nu3(27) = 3" $ nuP 3 27 `shouldBe` 3
    it "nu5(100) = 2" $ nuP 5 100 `shouldBe` 2

  describe "ecHomology p=2, even degree" $ do
    -- ecHomology p degree logh ac
    it "degree=2, logh=1, ac=6 has Z in degree 0" $
      groupInDegree 0 (ecHomology 2 2 1 6) `shouldBe` freeGroup 1

    it "degree=2, logh=1, ac=6 has Z/2 in degree 2" $
      groupInDegree 2 (ecHomology 2 2 1 6) `shouldBe` singletonGroup 2 1 1

    it "degree=2, logh=1, ac=6 has Z/4 in degree 4" $
      groupInDegree 4 (ecHomology 2 2 1 6) `shouldBe` singletonGroup 2 2 1

    it "degree=2, logh=1, ac=6 has Z/2 in degree 6" $
      groupInDegree 6 (ecHomology 2 2 1 6) `shouldBe` singletonGroup 2 1 1

    it "degree=2, logh=1, ac=6 is trivial in odd degrees" $
      groupInDegree 3 (ecHomology 2 2 1 6) `shouldBe` emptyGroup

  describe "ecHomology p=2, odd degree" $ do
    it "degree=1, logh=1, ac=5 has Z/2 in degree 1" $
      groupInDegree 1 (ecHomology 2 1 1 5) `shouldBe` singletonGroup 2 1 1

    it "degree=1, logh=1, ac=5 has Z/2 in degree 3" $
      groupInDegree 3 (ecHomology 2 1 1 5) `shouldBe` singletonGroup 2 1 1

    it "degree=1, logh=1, ac=5 is trivial in degree 2" $
      groupInDegree 2 (ecHomology 2 1 1 5) `shouldBe` emptyGroup

  describe "ecHomology p=3" $ do
    it "degree=2, logh=1, ac=6 has Z/3 in degree 2" $
      groupInDegree 2 (ecHomology 3 2 1 6) `shouldBe` singletonGroup 3 1 1

    it "degree=2, logh=1, ac=6 has Z/9 in degree 4 (nu3(2)=0, but 4/2=2, nu3(2)=0, so Z/3^1)" $
      groupInDegree 4 (ecHomology 3 2 1 6) `shouldBe` singletonGroup 3 1 1

    it "degree=2, logh=1, ac=9 has Z/9 in degree 6 (6/2=3, nu3(3)=1)" $
      groupInDegree 6 (ecHomology 3 2 1 9) `shouldBe` singletonGroup 3 2 1
