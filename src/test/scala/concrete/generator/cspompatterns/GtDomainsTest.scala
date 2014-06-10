package concrete.generator.cspompattern

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.CSPOMDriver._
import cspom.variable.IntVariable
import cspom.CSPOMConstraint
import concrete.generator.cspompatterns.GtDomains
import cspom.util.Interval
import cspom.util.RangeSet
import cspom.util.IntInterval
import cspom.util.IntRangeSet

class GtDomainsTest extends FlatSpec with Matchers {

  val v0 = IntVariable.free()
  val v1 = IntVariable(0 to 10)

  val ctr = CSPOMConstraint('gt, Seq(v0, v1))

  "GtDomains" should "filter" in {
    val ch = GtDomains.compiler(ctr).filter(t => t._1 != t._2)
    ch should have size 1
    ch(v0).asInstanceOf[IntVariable].domain shouldBe IntRangeSet(IntInterval.greaterThan(0))
  }

}