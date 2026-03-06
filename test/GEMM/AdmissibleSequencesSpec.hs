module GEMM.AdmissibleSequencesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import GEMM.AdmissibleSequences

spec :: Spec
spec = do
  describe "admissible" $ do
    it "(2,1) is admissible" $
      admissible (AdmissibleSeq [2, 1]) `shouldBe` True

    it "(1,2) is not admissible" $
      admissible (AdmissibleSeq [1, 2]) `shouldBe` False

    it "empty sequence is admissible" $
      admissible (AdmissibleSeq []) `shouldBe` True

    it "singleton is admissible" $
      admissible (AdmissibleSeq [5]) `shouldBe` True

  describe "stableDegree" $ do
    it "sum of elements" $
      stableDegree (AdmissibleSeq [4, 2, 1]) `shouldBe` 7

  describe "excess" $ do
    it "2*a0 - stableDegree" $
      excess (AdmissibleSeq [4, 2, 1]) `shouldBe` 1

  describe "genus" $ do
    it "genus 1 for a0=0" $
      genus (AdmissibleSeq [0]) `shouldBe` 1

    it "genus 1 for a0=1" $
      genus (AdmissibleSeq [1]) `shouldBe` 1

    it "genus 2 for even last" $
      genus (AdmissibleSeq [4, 2]) `shouldBe` 2

    it "genus 3 for odd last" $
      genus (AdmissibleSeq [4, 1]) `shouldBe` 3

  describe "generateSequences" $ do
    it "all generated sequences are admissible" $
      property $ \(Positive sd) ->
        let seqs = generateSequences sd 0 sd
        in all admissible seqs

    it "all generated sequences have correct stable degree" $
      property $ \(Positive sd) ->
        let seqs = generateSequences sd 0 sd
        in all (\s -> stableDegree s == sd) seqs

    it "sd=0 excess=0 gives one empty sequence (stableDegree=-1)" $
      let seqs = generateSequences 0 0 0
      in (seqs, map stableDegree seqs) `shouldBe` ([AdmissibleSeq []], [-1])

    it "sd=1 excess=1 gives [(1)]" $
      generateSequences 1 1 1 `shouldBe` [AdmissibleSeq [1]]

    it "sd=3 excess range 0..2 generates known count" $
      length (generateSequences 3 0 3) `shouldBe` 2

  describe "filterByEvenFirst" $ do
    it "keeps sequences with even first element" $
      let seqs = [AdmissibleSeq [2, 1], AdmissibleSeq [3]]
      in filterByEvenFirst seqs `shouldBe` [AdmissibleSeq [2, 1]]

    it "keeps empty sequences" $
      filterByEvenFirst [AdmissibleSeq []] `shouldBe` [AdmissibleSeq []]

  describe "firstCech" $ do
    it "removes first element" $
      firstCech (AdmissibleSeq [4, 2, 1]) `shouldBe` AdmissibleSeq [2, 1]

    it "empty stays empty" $
      firstCech (AdmissibleSeq []) `shouldBe` AdmissibleSeq []
