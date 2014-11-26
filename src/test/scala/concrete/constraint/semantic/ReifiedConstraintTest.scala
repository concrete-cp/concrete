package concrete.constraint.semantic

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.Revised
import concrete.BooleanDomain
import concrete.IntDomain
import concrete.TRUE

class ReifiedConstraintTest extends FlatSpec with Matchers {

  "Reified neq" should "be revised after advise" in {
    val v0 = new Variable("v0", IntDomain(0))
    val v1 = new Variable("v1", IntDomain(3, 4))
    val c1 = new ReifiedConstraint(
      new Variable("control", BooleanDomain()),
      new Neq(v0, v1),
      new EqAC(v0, v1))

    val c2 = new ReifiedConstraint(
      new Variable("control", BooleanDomain()),
      new Neq(v0, v1),
      new EqBC(v0, v1))

    val ac = new AdviseCount
    c1.register(ac)
    c2.register(ac)
    val domains = c1.scope.map(_.initDomain)

    c1.advise(domains, 2) should be < 0
    c2.advise(domains, 2) should be >= 0

    val Revised(mod, _, _) = c1.revise(domains, c1.initState)
    val Revised(m2, entail, _) = c2.revise(mod, c2.initState)

    m2(0) shouldBe TRUE
    assert(entail)
  }

}