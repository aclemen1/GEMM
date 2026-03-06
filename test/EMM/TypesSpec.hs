module EMM.TypesSpec (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IM
import EMM.Types

spec :: Spec
spec = do
  describe "Group monoid" $ do
    it "mempty is the trivial group" $
      unGroup emptyGroup `shouldBe` IM.empty

    it "singletonGroup creates a group with one component" $
      unGroup (singletonGroup 2 3) `shouldBe` IM.singleton 2 3

    it "singletonGroup with power 0 gives empty" $
      singletonGroup 2 0 `shouldBe` emptyGroup

    it "mappend merges by adding powers" $
      let g1 = singletonGroup 1 2
          g2 = singletonGroup 1 3
      in unGroup (g1 <> g2) `shouldBe` IM.singleton 1 5

    it "mappend keeps distinct logorders separate" $
      let g = singletonGroup 1 2 <> singletonGroup 2 3
      in unGroup g `shouldBe` IM.fromList [(1,2),(2,3)]

  describe "GradedGroup" $ do
    it "groupInDegree returns empty for missing degrees" $
      groupInDegree 5 mempty `shouldBe` emptyGroup

    it "anticonnexity returns max degree" $
      let gg = GradedGroup (IM.fromList [(0, singletonGroup 0 1), (5, singletonGroup 1 1)])
      in anticonnexity gg `shouldBe` 5

  describe "showGroup" $ do
    it "shows trivial group" $
      showGroup emptyGroup `shouldBe` "(0)"

    it "shows Z" $
      showGroup (singletonGroup 0 1) `shouldBe` "Z"

    it "shows Z/2" $
      showGroup (singletonGroup 1 1) `shouldBe` "Z/2^1"
