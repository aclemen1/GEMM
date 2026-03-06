module GEMM.TypesSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import GEMM.Types

spec :: Spec
spec = do
  describe "Group monoid" $ do
    it "mempty is the trivial group" $
      unGroup emptyGroup `shouldBe` Map.empty

    it "singletonGroup creates a group with one component" $
      unGroup (singletonGroup 2 3 1) `shouldBe` Map.singleton (2, 3) 1

    it "singletonGroup with power 0 gives empty" $
      singletonGroup 2 1 0 `shouldBe` emptyGroup

    it "freeGroup creates Z^r" $
      unGroup (freeGroup 3) `shouldBe` Map.singleton (0, 0) 3

    it "mappend merges by adding powers" $
      let g1 = singletonGroup 2 1 2
          g2 = singletonGroup 2 1 3
      in unGroup (g1 <> g2) `shouldBe` Map.singleton (2, 1) 5

    it "mappend keeps distinct components separate" $
      let g = singletonGroup 2 1 2 <> singletonGroup 2 2 3
      in unGroup g `shouldBe` Map.fromList [((2,1),2),((2,2),3)]

    it "mappend keeps different primes separate" $
      let g = singletonGroup 2 1 1 <> singletonGroup 3 1 1
      in unGroup g `shouldBe` Map.fromList [((2,1),1),((3,1),1)]

  describe "GradedGroup" $ do
    it "groupInDegree returns empty for missing degrees" $
      groupInDegree 5 mempty `shouldBe` emptyGroup

    it "anticonnexity returns max degree" $
      let gg = GradedGroup (IM.fromList [(0, freeGroup 1), (5, singletonGroup 2 1 1)])
      in anticonnexity gg `shouldBe` 5

  describe "showGroup" $ do
    it "shows trivial group" $
      showGroup emptyGroup `shouldBe` "(0)"

    it "shows Z" $
      showGroup (freeGroup 1) `shouldBe` "Z"

    it "shows Z/2" $
      showGroup (singletonGroup 2 1 1) `shouldBe` "Z/2"

    it "shows Z/2^3" $
      showGroup (singletonGroup 2 3 1) `shouldBe` "Z/2^3"

    it "shows (Z/3)^5" $
      showGroup (singletonGroup 3 1 5) `shouldBe` "(Z/3)^5"

    it "shows mixed group" $
      showGroup (freeGroup 1 <> singletonGroup 2 1 2)
        `shouldBe` "Z + (Z/2)^2"
