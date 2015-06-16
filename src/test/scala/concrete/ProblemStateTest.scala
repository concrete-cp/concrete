package concrete

import org.scalatest.Matchers
import org.scalatest.FlatSpec

/**
 * @author vion
 */
class ProblemStateTest extends FlatSpec with Matchers {
  "ProblemState" should "store entailment information" in {
    val pb = Problem()
    val ps = ProblemState(pb).entail(203).entail(202)
    assert(ps.isEntailed(203))
    assert(ps.isEntailed(202))
    val ps2 = ps.entail(134)
    withClue(ps2) {
      assert(ps2.isEntailed(203))
      assert(ps2.isEntailed(202))
      assert(ps2.isEntailed(134))
    }
  }

}