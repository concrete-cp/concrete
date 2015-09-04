package concrete.heuristic

import concrete.IntDomain
import concrete.Variable
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.ParameterManager
import concrete.Problem
import org.scalatest.OptionValues

/**
 * @author vion
 */
class MaxRegretTest extends FlatSpec with Matchers with OptionValues {
  "MaxRegret" should "select correct variable" in {
    val v1 = new Variable("v1", IntDomain.ofSeq(1, 5, 6))
    val v2 = new Variable("v2", IntDomain.ofSeq(1, 3, 6))

    val p = Problem(v1, v2)
    val ps = p.initState.toState
    val h = new MaxRegret(new ParameterManager, p.variables)

    h.select(ps).value shouldBe v1
  }
}