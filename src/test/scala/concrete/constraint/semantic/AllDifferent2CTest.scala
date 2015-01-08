package concrete.constraint.semantic

import concrete.IntDomain
import concrete.Variable
import concrete.UNSATException
import concrete.constraint.AdviseCount
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.Contradiction
import concrete.Problem

class AllDifferent2CTest extends FlatSpec with Matchers {

  "AllDifferent2C" should "detect contradiction" in {
    val v1 = new Variable("1", IntDomain(7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9))

    val c = new AllDifferent2C(v1, v2, v3, v4, v5)
    val ps = Problem(v1, v2, v3, v4, v5).initState

    c.register(new AdviseCount)
    c.adviseAll(ps)

    c.revise(ps) shouldBe Contradiction
  }

  it should "check correctly" in {
    val v1 = new Variable("1", IntDomain(6, 7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9, 10))

    val c = new AllDifferent2C(v1, v2, v3, v4, v5)

    assert(c.check(Array(7, 6, 9, 8, 10)))
    assert(!c.check(Array(7, 6, 7, 8, 10)))

  }
}