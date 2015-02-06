package concrete.constraint.semantic

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.IntDomain
import concrete.Problem
import concrete.ProblemState
import concrete.Variable

final class AbsIntTest extends FlatSpec with Matchers {

  "AbsInt" should "filter X" in {

    val x = new Variable("x", IntDomain(-100 to 100))
    val y = new Variable("y", IntDomain.ofSeq(-5))

    val c = new AbsBC(x, y)

    val ps = Problem(x, y).initState
    c.adviseAll(ps)

    assert(c.intervalsOnly(ps))

    val mod: ProblemState = c.consistentRevise(ps)

    mod.dom(x) should not be theSameInstanceAs(ps.dom(x))
    mod.dom(y) should be theSameInstanceAs ps.dom(y)

    mod.dom(x) shouldBe Seq(5)

    assert(c.intervalsOnly(mod))

  }

  it should "filter Y" in {

    val x = new Variable("x", IntDomain.ofSeq(7))
    val y = new Variable("y", IntDomain(-100 to 100))

    val c = new AbsAC(x, y)

    val ps = Problem(x, y).initState
    c.adviseAll(ps)
    assert(c.intervalsOnly(ps))
    val mod = c.consistentRevise(ps)

    mod.dom(x) should be theSameInstanceAs ps.dom(x)
    mod.dom(y) should not be theSameInstanceAs(ps.dom(y))

    mod.dom(y) shouldBe Seq(-7, 7)

    assert(!c.intervalsOnly(mod))
    assert(mod.dom(x).bound)
  }

}