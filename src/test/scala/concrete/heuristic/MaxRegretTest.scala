package concrete.heuristic

import concrete.{IntDomain, Problem, Variable}
import concrete.heuristic.variable.MaxRegret
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author vion
  */
class MaxRegretTest extends AnyFlatSpec with Matchers with OptionValues {
  "MaxRegret" should "select correct variable" in {
    val v1 = new Variable("v1", IntDomain.ofSeq(1, 5, 6))
    val v2 = new Variable("v2", IntDomain.ofSeq(1, 3, 6))

    val p = Problem(v1, v2)

    val h = new MaxRegret(p.variables)
    val ps = h.compute(null, p.initState.toState)
    h.select(ps, p.variables).headOption.value shouldBe v1
  }
}