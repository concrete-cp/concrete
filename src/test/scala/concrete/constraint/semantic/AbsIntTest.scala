package concrete.constraint.semantic

import scala.Traversable

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.IntDomain
import concrete.Revised
import concrete.Variable

final class AbsIntTest extends FlatSpec with Matchers {

  "AbsInt" should "filter X" in {

    val x = new Variable("x", IntDomain(-100 to 100))
    val y = new Variable("y", IntDomain(-5))

    val c = new AbsBC(x, y)

    val domains = Array(x.initDomain, y.initDomain)
    c.adviseAll(domains)

    assert(c.intervalsOnly(domains))

    val Revised(mod, _, _) = c.revise(domains, c.initState)

    mod(0) should not be theSameInstanceAs(domains(0))
    mod(1) should be theSameInstanceAs domains(1)

    mod(0) shouldBe Seq(5)

    assert(c.intervalsOnly(mod))

  }

  it should "filter Y" in {

    val x = new Variable("x", IntDomain(7))
    val y = new Variable("y", IntDomain(-100 to 100))

    val c = new AbsAC(x, y)

    val domains = Array(x.initDomain, y.initDomain)
    c.adviseAll(domains)
    assert(c.intervalsOnly(domains))
    val Revised(mod, _, _) = c.revise(domains, Unit)

    mod(0) should be theSameInstanceAs domains(0)
    mod(1) should not be theSameInstanceAs(domains(1))

    mod(1) shouldBe Seq(-7, 7)

    assert(!c.intervalsOnly(mod))
    assert(mod(0).bound)
  }

}