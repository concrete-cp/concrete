package concrete.generator.cspompatterns

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.CSPOMDriver.CSPOMIntExpressionOperations
import cspom.CSPOM
import cspom.CSPOM.ctr
import cspom.compiler.CSPOMCompiler
import cspom.util.IntInterval
import cspom.util.RangeSet
import cspom.variable.IntExpression
import cspom.variable.IntVariable
import cspom.compiler.MergeEq

class SumDomainsTest extends FlatSpec with Matchers {

  "SumDomains" should "filter >" in {
    val cspom = CSPOM { implicit p =>
      val v0 = IntVariable.free() as "V0"
      val v1 = IntVariable(0 to 10)
      ctr(v0 > v1)
    }

    CSPOMCompiler.compile(cspom, Seq(MergeEq, SumDomains))

    val Some(v0: IntVariable) = cspom.variable("V0")
    IntExpression.implicits.ranges(v0) shouldBe RangeSet(IntInterval.atLeast(1))
  }

  it should "filter >=" in {
    val cspom = CSPOM { implicit p =>
      val v0 = IntVariable.free() as "V0"
      val v1 = IntVariable(0 to 10)
      ctr(v0 >= v1)
    }

    CSPOMCompiler.compile(cspom, Seq(MergeEq, SumDomains))

    val Some(v0: IntVariable) = cspom.variable("V0")
    IntExpression.implicits.ranges(v0) shouldBe RangeSet(IntInterval.atLeast(0))

  }

}