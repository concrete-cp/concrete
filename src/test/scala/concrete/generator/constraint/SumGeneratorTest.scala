package concrete.generator.constraint

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.CSPOMDriver.sumProd
import concrete.generator.FailedGenerationException
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.SumDomains
import concrete.util.Interval
import cspom.CSPOM
import cspom.CSPOM.ctr
import cspom.compiler.MergeEq
import cspom.compiler.CSPOMCompiler
import cspom.variable.CSPOMConstant
import cspom.variable.IntVariable
import org.scalatest.TryValues

class SumGeneratorTest extends FlatSpec with Matchers with TryValues {

  "SumDomains" should "filter" in {
    val cspom = CSPOM { implicit problem =>
      val r = sumProd((1, IntVariable(0 to 5)), (2, IntVariable(10 to 15)), (3, CSPOMConstant(20))) as "test"
    }

    CSPOMCompiler.compile(cspom, Seq(SumDomains))

    var problem = new ProblemGenerator().generate(cspom).success.value._1

    problem.variable("test").initDomain.span shouldBe Interval(80, 95)

  }

  it should "filter and merge" in {
    val cspom = CSPOM { implicit problem =>
      ctr(sumProd((1, IntVariable(0 to 5)), (2, IntVariable.free() as "test"), (3, CSPOMConstant(20))) === CSPOMConstant(5))
    }

    CSPOMCompiler.compile(cspom, Seq(SumDomains, MergeEq))

    var problem = new ProblemGenerator().generate(cspom).success.value._1

    problem.variable("test").initDomain.span shouldBe Interval(-30, -28)
  }
}