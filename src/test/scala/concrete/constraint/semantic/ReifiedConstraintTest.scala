package concrete.constraint.semantic

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.BooleanDomain
import concrete.IntDomain
import concrete.TRUE
import concrete.Problem
import concrete.UNKNOWNBoolean

class ReifiedConstraintTest extends FlatSpec with Matchers {

  "Reified neq" should "be revised after advise" in {
    val v0 = new Variable("v0", IntDomain.ofSeq(2))
    val v1 = new Variable("v1", IntDomain.ofSeq(0, 4))
    val control1 = new Variable("control1", BooleanDomain())
    val control2 = new Variable("control2", BooleanDomain())
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

    val pb = Problem(control1, control2, v0, v1)
    pb.addConstraints(Seq(c1, c2))
    val ps = pb.initState

    c1.advise(ps, 2) should be >= 0
    c2.advise(ps, 2) should be >= 0

    val m2 = c2.consistentRevise(ps)

    val m1 = c1.consistentRevise(ps)

    m1.dom(control1) shouldBe TRUE
    assert(m1.isEntailed(c1))

    m2.dom(control2) shouldBe UNKNOWNBoolean
    assert(!m2.isEntailed(c2))
  }

}