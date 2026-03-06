import Test.Hspec

import qualified EMM.TypesSpec
import qualified EMM.GradedGroupsSpec
import qualified EMM.AdmissibleSequencesSpec
import qualified EMM.ElementaryComplexSpec
import qualified EMM.EilenbergMacLaneSpec

main :: IO ()
main = hspec $ do
  describe "EMM.Types" EMM.TypesSpec.spec
  describe "EMM.GradedGroups" EMM.GradedGroupsSpec.spec
  describe "EMM.AdmissibleSequences" EMM.AdmissibleSequencesSpec.spec
  describe "EMM.ElementaryComplex" EMM.ElementaryComplexSpec.spec
  describe "EMM.EilenbergMacLane" EMM.EilenbergMacLaneSpec.spec
