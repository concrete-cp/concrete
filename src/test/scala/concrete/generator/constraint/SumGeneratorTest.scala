package concrete.generator.constraint

import org.junit.Test
import cspom.CSPOM
import concrete.CSPOMDriver._
import cspom.variable.IntVariable
import concrete.generator.ProblemGenerator
import cspom.variable.CSPOMConstant
import org.junit.Assert.assertEquals
import concrete.util.Interval
import cspom.CSPOM.ctr
import cspom.compiler.ProblemCompiler
import cspom.compiler.MergeEq
import concrete.generator.cspompatterns.SumDomains
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class SumGeneratorTest extends FlatSpec with Matchers {

  "SumDomains" should "filter" in {
    val cspom = CSPOM { implicit problem =>
      val r = sumProd((1, IntVariable(0 to 5)), (2, IntVariable(10 to 15)), (3, CSPOMConstant(20))) as "test"
    }

    ProblemCompiler.compile(cspom, Seq(SumDomains))

    var problem = new ProblemGenerator().generate(cspom)._1

    problem.variable("test").initDomain.span shouldBe Interval(80, 95)

  }

  it should "filter and merge" in {
    val cspom = CSPOM { implicit problem =>
      ctr(sumProd((1, IntVariable(0 to 5)), (2, IntVariable.free() as "test"), (3, CSPOMConstant(20))) === CSPOMConstant(5))
    }

    ProblemCompiler.compile(cspom, Seq(SumDomains, MergeEq))

    var problem = new ProblemGenerator().generate(cspom)._1

    problem.variable("test").initDomain.span shouldBe Interval(-30, -28)
  }
}