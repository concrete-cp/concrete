package concrete.constraint.semantic

import scala.annotation.tailrec
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.Contradiction
import concrete.IntDomain
import concrete.Variable
import concrete.Problem

class AllDifferentBCTest extends FlatSpec with Matchers {

  "AllDifferentBC" should "detect contradiction" in {
    val v1 = new Variable("1", IntDomain(7))
    val v2 = new Variable("2", IntDomain(6))
    val v3 = new Variable("3", IntDomain(7, 9))
    val v4 = new Variable("4", IntDomain(8))
    val v5 = new Variable("5", IntDomain(8, 9))

    val c = new AllDifferentBC(v1, v2, v3, v4, v5)
    val ps = Problem(v1, v2, v3, v4, v5).initState

    c.revise(ps) shouldBe Contradiction
  }

}