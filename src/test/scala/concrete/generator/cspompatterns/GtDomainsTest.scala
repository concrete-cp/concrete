package concrete.generator.cspompattern

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.CSPOMDriver._
import concrete.generator.cspompatterns.GtDomains
import cspom.CSPOMConstraint
import cspom.util.IntInterval
import cspom.util.IntRangeSet
import cspom.variable.IntVariable
import concrete.generator.cspompatterns.GeDomains

class GtDomainsTest extends FlatSpec with Matchers {

  val v0 = IntVariable.free()
  val v1 = IntVariable(0 to 10)

  "GtDomains" should "filter" in {
    val ctr = CSPOMConstraint('gt, Seq(v0, v1))
    val ch = GtDomains.compiler(ctr).filter(t => t._1 != t._2)
    ch should have size 1
    ch(v0).asInstanceOf[IntVariable].domain shouldBe IntRangeSet(IntInterval.atLeast(1))
  }

  "GeDomains" should "filter" in {
    val ctr = CSPOMConstraint('ge, Seq(v0, v1))
    val ch = GeDomains.compiler(ctr).filter(t => t._1 != t._2)
    ch should have size 1
    ch(v0).asInstanceOf[IntVariable].domain shouldBe IntRangeSet(IntInterval.atLeast(0))
  }

}