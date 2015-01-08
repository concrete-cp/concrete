package concrete.constraint.semantic

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.BooleanDomain
import concrete.IntDomain
import concrete.TRUE
import concrete.Problem

class ReifiedConstraintTest extends FlatSpec with Matchers {

  "Reified neq" should "be revised after advise" in {
    val v0 = new Variable("v0", IntDomain(0))
    val v1 = new Variable("v1", IntDomain(3, 4))
    val control1 = new Variable("control1", BooleanDomain())
    val control2 = new Variable("control1", BooleanDomain())
    val c1 = new ReifiedConstraint(
      control1,
      new Neq(v0, v1),
      new EqAC(v0, v1))

    val c2 = new ReifiedConstraint(
      control2,
      new Neq(v0, v1),
      new EqBC(v0, v1))

    val ac = new AdviseCount
    c1.register(ac)
    c2.register(ac)

    val ps = Problem(control1, control2, v0, v1).initState

    c1.advise(ps, 2) should be < 0
    c2.advise(ps, 2) should be >= 0

    val mod = c1.consistentRevise(ps)
    mod shouldBe ps
    val m2 = c2.consistentRevise(mod)

    m2.dom(control2) shouldBe TRUE
    assert(m2.isEntailed(c2))
  }

}