module EMM.EilenbergMacLaneSpec (spec) where

import Test.Hspec
import EMM.Types
import EMM.EilenbergMacLane

spec :: Spec
spec = do
  describe "K(Z/2, 1) = RP^infty" $ do
    let h = emHomology 1 1 10

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` freeGroup 1

    it "H_1 = Z/2" $
      groupInDegree 1 h `shouldBe` singletonGroup 2 1 1

    it "H_2 = 0 (integral homology of RP^infty)" $
      groupInDegree 2 h `shouldBe` emptyGroup

    it "H_3 = Z/2" $
      groupInDegree 3 h `shouldBe` singletonGroup 2 1 1

  describe "K(Z/2, 2)" $ do
    let h = emHomology 1 2 10

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` freeGroup 1

    it "H_1 = 0" $
      groupInDegree 1 h `shouldBe` emptyGroup

    it "H_2 = Z/2" $
      groupInDegree 2 h `shouldBe` singletonGroup 2 1 1

  describe "K(Z/2, 2) via emHomologyP" $ do
    let h = emHomologyP 2 1 2 10

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` freeGroup 1

    it "H_2 = Z/2" $
      groupInDegree 2 h `shouldBe` singletonGroup 2 1 1

  describe "K(Z, 2) = CP^infty" $ do
    let h = emHomologyZ 2 10

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` freeGroup 1

    it "H_1 = 0" $
      groupInDegree 1 h `shouldBe` emptyGroup

    it "H_2 = Z" $
      groupInDegree 2 h `shouldBe` freeGroup 1

    it "H_4 = Z" $
      groupInDegree 4 h `shouldBe` freeGroup 1

    it "H_3 = 0" $
      groupInDegree 3 h `shouldBe` emptyGroup

    it "H_6 = Z (no torsion)" $
      groupInDegree 6 h `shouldBe` freeGroup 1

    it "H_5 = 0 (no torsion)" $
      groupInDegree 5 h `shouldBe` emptyGroup

  describe "K(Z, 3)" $ do
    let h = emHomologyZ 3 15

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` freeGroup 1

    it "H_3 = Z" $
      groupInDegree 3 h `shouldBe` freeGroup 1

    it "H_4 = 0" $
      groupInDegree 4 h `shouldBe` emptyGroup

    it "H_5 = Z/2 (first 2-torsion at n-2+2p = 5)" $
      groupInDegree 5 h `shouldBe` singletonGroup 2 1 1

    it "H_6 = 0" $
      groupInDegree 6 h `shouldBe` emptyGroup

    it "H_7 = Z/3 (first 3-torsion at n-2+2p = 7)" $
      groupInDegree 7 h `shouldBe` singletonGroup 3 1 1

  -- =========================================================================
  -- K(Z/3, n) — odd prime validation
  -- =========================================================================

  describe "K(Z/3, 1) = BZ/3 (lens space)" $ do
    let h = emHomologyP 3 1 1 12

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` freeGroup 1

    it "H_{2k-1} = Z/3 for k=1..6" $
      mapM_ (\k -> groupInDegree (2*k - 1) h `shouldBe` singletonGroup 3 1 1) [1..6]

    it "H_{2k} = 0 for k=1..6" $
      mapM_ (\k -> groupInDegree (2*k) h `shouldBe` emptyGroup) [1..6]

  describe "K(Z/5, 1) = BZ/5 (lens space)" $ do
    let h = emHomologyP 5 1 1 12

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` freeGroup 1

    it "H_{2k-1} = Z/5 for k=1..6" $
      mapM_ (\k -> groupInDegree (2*k - 1) h `shouldBe` singletonGroup 5 1 1) [1..6]

    it "H_{2k} = 0 for k=1..6" $
      mapM_ (\k -> groupInDegree (2*k) h `shouldBe` emptyGroup) [1..6]

  describe "K(Z/7, 1) = BZ/7 (lens space)" $ do
    let h = emHomologyP 7 1 1 14

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` freeGroup 1

    it "H_{2k-1} = Z/7 for k=1..7" $
      mapM_ (\k -> groupInDegree (2*k - 1) h `shouldBe` singletonGroup 7 1 1) [1..7]

    it "H_{2k} = 0 for k=1..7" $
      mapM_ (\k -> groupInDegree (2*k) h `shouldBe` emptyGroup) [1..7]

  describe "K(Z/3, 2)" $ do
    let h = emHomologyP 3 1 2 10

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` freeGroup 1

    it "H_1 = 0" $
      groupInDegree 1 h `shouldBe` emptyGroup

    it "H_2 = Z/3 (fundamental class)" $
      groupInDegree 2 h `shouldBe` singletonGroup 3 1 1

    it "H_3 = 0 (Whitehead: H_{n+1} = 0 for n >= 2)" $
      groupInDegree 3 h `shouldBe` emptyGroup

  describe "K(Z/3, 3)" $ do
    let h = emHomologyP 3 1 3 12

    it "H_0 = Z" $
      groupInDegree 0 h `shouldBe` freeGroup 1

    it "H_3 = Z/3 (fundamental class)" $
      groupInDegree 3 h `shouldBe` singletonGroup 3 1 1

    it "H_4 = 0 (Whitehead: H_{n+1} = 0 for n >= 2)" $
      groupInDegree 4 h `shouldBe` emptyGroup

    it "H_5 = 0 (Whitehead: H_{n+2} = G ⊗ Z/2 = 0 since gcd(3,2)=1, n >= 3)" $
      groupInDegree 5 h `shouldBe` emptyGroup

  -- =========================================================================
  -- Whitehead exact sequence: H_{n+1} = 0, H_{n+2} = G ⊗ Z/2 for n >= 3
  -- =========================================================================

  describe "Whitehead: H_{n+1}(K(G,n)) = 0 for n >= 2" $ do
    it "K(Z/3, 4): H_5 = 0" $
      groupInDegree 5 (emHomologyP 3 1 4 6) `shouldBe` emptyGroup

    it "K(Z, 4): H_5 = 0" $
      groupInDegree 5 (emHomologyZ 4 6) `shouldBe` emptyGroup

    it "K(Z, 5): H_6 = 0" $
      groupInDegree 6 (emHomologyZ 5 7) `shouldBe` emptyGroup

  describe "Whitehead: H_{n+2}(K(Z,n)) = Z/2 for n >= 3" $ do
    it "K(Z, 3): H_5 = Z/2" $
      groupInDegree 5 (emHomologyZ 3 6) `shouldBe` singletonGroup 2 1 1

    it "K(Z, 4): H_6 = Z/2" $
      groupInDegree 6 (emHomologyZ 4 7) `shouldBe` singletonGroup 2 1 1

    it "K(Z, 5): H_7 = Z/2" $
      groupInDegree 7 (emHomologyZ 5 8) `shouldBe` singletonGroup 2 1 1

  describe "Whitehead: H_{n+2}(K(Z/3,n)) = 0 for n >= 3 (since Z/3 ⊗ Z/2 = 0)" $ do
    it "K(Z/3, 3): H_5 = 0" $
      groupInDegree 5 (emHomologyP 3 1 3 6) `shouldBe` emptyGroup

    it "K(Z/3, 4): H_6 = 0" $
      groupInDegree 6 (emHomologyP 3 1 4 7) `shouldBe` emptyGroup

  -- =========================================================================
  -- First p-torsion in K(Z, n) at degree n + 2(p-1)
  -- =========================================================================

  describe "First p-torsion in K(Z, n) at degree n + 2(p-1)" $ do
    -- K(Z, 4)
    it "K(Z, 4): first Z/2 at degree 6 = 4+2(2-1)" $
      groupInDegree 6 (emHomologyZ 4 7) `shouldBe` singletonGroup 2 1 1

    it "K(Z, 4): first Z/3 at degree 8 = 4+2(3-1)" $
      groupInDegree 8 (emHomologyZ 4 9) `shouldBe`
        freeGroup 1 <> singletonGroup 3 1 1

    it "K(Z, 4): first Z/5 at degree 12 = 4+2(5-1)" $
      let h = emHomologyZ 4 13
      in groupInDegree 12 h `shouldBe`
           freeGroup 1 <> singletonGroup 2 2 1 <> singletonGroup 5 1 1

    -- K(Z, 5)
    it "K(Z, 5): first Z/2 at degree 7 = 5+2(2-1)" $
      groupInDegree 7 (emHomologyZ 5 8) `shouldBe` singletonGroup 2 1 1

    it "K(Z, 5): first Z/3 at degree 9 = 5+2(3-1)" $
      groupInDegree 9 (emHomologyZ 5 10) `shouldBe`
        singletonGroup 2 1 1 <> singletonGroup 3 1 1

    it "K(Z, 5): first Z/5 at degree 13 = 5+2(5-1)" $
      let h = emHomologyZ 5 14
      in groupInDegree 13 h `shouldBe`
           singletonGroup 2 1 1 <> singletonGroup 3 1 1 <> singletonGroup 5 1 1
