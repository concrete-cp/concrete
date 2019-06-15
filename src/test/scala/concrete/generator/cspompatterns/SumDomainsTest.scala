package concrete.generator.cspompatterns

import concrete.CSPOMDriver._
import cspom.CSPOM
import cspom.CSPOM.ctr
import cspom.compiler.{CSPOMCompiler, MergeEq}
import cspom.util.{IntInterval, RangeSet}
import cspom.variable.{IntExpression, IntVariable}
import org.scalatest.{FlatSpec, Matchers, OptionValues}

class SumDomainsTest extends FlatSpec with Matchers with OptionValues {

  "SumDomains" should "filter >" in {
    val cspom = CSPOM { implicit p =>
      val v0 = IntVariable.free() as "V0"
      val v1 = IntVariable(0 to 10)
      ctr(v0 > v1)
    }

    CSPOMCompiler.compile(cspom, Seq(MergeEq, SumDomains))

    val v0 = cspom.variable("V0").value
    IntExpression.implicits.ranges(v0) shouldBe RangeSet(IntInterval.atLeast(1))
  }

  it should "filter >=" in {
    val cspom = CSPOM { implicit p =>
      val v0 = IntVariable.free() as "V0"
      val v1 = IntVariable(0 to 10)
      ctr(v0 >= v1)
    }

    CSPOMCompiler.compile(cspom, Seq(MergeEq, SumDomains))

    val v0 = cspom.variable("V0").value
    IntExpression.implicits.ranges(v0) shouldBe RangeSet(IntInterval.atLeast(0))

  }

//  it should "filter !=" in {
//    val cspom = CSPOM { implicit p =>
//      val v0 = IntVariable(IntInterval.atLeast(0)) as "V0"
//      val v1 = CSPOMConstant(0)
//      ctr(1 *: v0 + 1 *: v1 !== 0)
//    }
//
//    CSPOMCompiler.compile(cspom, Seq(MergeEq, SumDomains, SumConstants)).get
//
//    val Some(v0: IntVariable) = cspom.variable("V0")
//    withClue(cspom) {
//      IntExpression.implicits.ranges(v0) shouldBe RangeSet(IntInterval.atLeast(1))
//      cspom.constraints shouldBe empty
//    }
//  }

}