import Test.Hspec

import qualified GEMM.TypesSpec
import qualified GEMM.GradedGroupsSpec
import qualified GEMM.AdmissibleSequencesSpec
import qualified GEMM.ElementaryComplexSpec
import qualified GEMM.EilenbergMacLaneSpec

main :: IO ()
main = hspec $ do
  describe "GEMM.Types" GEMM.TypesSpec.spec
  describe "GEMM.GradedGroups" GEMM.GradedGroupsSpec.spec
  describe "GEMM.AdmissibleSequences" GEMM.AdmissibleSequencesSpec.spec
  describe "GEMM.ElementaryComplex" GEMM.ElementaryComplexSpec.spec
  describe "GEMM.EilenbergMacLane" GEMM.EilenbergMacLaneSpec.spec
