package concrete.generator.constraint

import scala.reflect.runtime.universe

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.CSPOMDriver.sumProd
import concrete.generator.ProblemGenerator
import concrete.generator.cspompatterns.SumConstants
import concrete.generator.cspompatterns.SumDomains
import concrete.util.Interval
import cspom.CSPOM
import cspom.CSPOM.constantSeq
import cspom.CSPOM.ctr
import cspom.CSPOM.seq2CSPOMSeq
import cspom.CSPOMConstraint
import cspom.compiler.CSPOMCompiler
import cspom.compiler.MergeEq
import cspom.variable.CSPOMConstant
import cspom.variable.IntVariable

class SumGeneratorTest extends FlatSpec with Matchers {

  "SumDomains" should "filter" in {
    val cspom = CSPOM { implicit problem =>
      val r = sumProd((1, IntVariable(0 to 5)), (2, IntVariable(10 to 15)), (3, CSPOMConstant(20))) as "test"
    }

    CSPOMCompiler.compile(cspom, Seq(SumDomains, SumConstants))

    var problem = new ProblemGenerator().generate(cspom).get._1

    problem.variable("test").initDomain.span shouldBe Interval(80, 95)

  }

  it should "filter and merge" in {
    val cspom = CSPOM { implicit problem =>
      ctr(sumProd((1, IntVariable(0 to 5)), (2, IntVariable.free() as "test"), (3, CSPOMConstant(20))) === CSPOMConstant(5))
    }

    CSPOMCompiler.compile(cspom, Seq(SumDomains, SumConstants, MergeEq))

    var problem = new ProblemGenerator().generate(cspom).get._1

    problem.variable("test").initDomain.span shouldBe Interval(-30, -28)
  }

  "SumGenerator" should "generate AC/BC variants" in {
    val cspom = CSPOM { implicit problem =>
      val v0 = IntVariable(0 to 10)
      val v1 = IntVariable(0 to 10)
      val r = problem.defineBool { r => CSPOMConstraint(r)('sum)(Seq(-1, 1), Seq(v0, v1), CSPOMConstant(0)) withParam ("mode" -> "eq") } as "r"
    }

    CSPOMCompiler.compile(cspom, Seq(MergeEq)).get

    val (problem, variables) = new ProblemGenerator().generate(cspom).get

    withClue(problem.toString(problem.initState.toState)) {
      problem.constraints.size shouldBe 2
    }

  }
}