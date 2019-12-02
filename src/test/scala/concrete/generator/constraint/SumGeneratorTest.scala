package concrete.generator.constraint

import concrete.CSPOMDriver._
import concrete.generator.cspompatterns.{SumConstants, SumDomains}
import concrete.generator.{ProblemGenerator, SumGenerator}
import concrete.util.Interval
import cspom.CSPOM.{constantSeq, ctr, seq2CSPOMSeq}
import cspom.compiler.{CSPOMCompiler, MergeEq}
import cspom.variable.{CSPOMConstant, IntVariable}
import cspom.{CSPOM, CSPOMConstraint}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SumGeneratorTest extends AnyFlatSpec with Matchers {

  "SumDomains" should "filter" in {
    val cspom = CSPOM { implicit problem =>
      sumProd((1, IntVariable(0 to 5)), (2, IntVariable(10 to 15)), (3, CSPOMConstant(20))) as "test"
    }

    CSPOMCompiler.compile(cspom, Seq(SumDomains, SumConstants))

    val problem = new ProblemGenerator().generate(cspom).get._1

    problem.variable("test").initDomain.span shouldBe Interval(80, 95)

  }

  it should "filter and merge" in {
    val cspom = CSPOM { implicit problem =>
      ctr(sumProd((1, IntVariable(0 to 5)), (2, IntVariable.free() as "test"), (3, CSPOMConstant(20))) === CSPOMConstant(5))
    }

    CSPOMCompiler.compile(cspom, Seq(SumDomains, SumConstants, MergeEq))

    val problem = new ProblemGenerator().generate(cspom).get._1

    problem.variable("test").initDomain.span shouldBe Interval(-30, -28)
  }

  it should "detect Sums" in {
    var c: CSPOMConstraint[_] = null

    CSPOM { implicit problem =>
      c = linear(
        Seq(IntVariable(0 to 5), IntVariable(0 to 5)),
        Seq(1, 2),
        "eq",
        10)
      ctr(c)
    }

    val (vars, coefs, constant, mode) = SumGenerator.readCSPOM(c)
    coefs shouldBe Seq(1, 2)
  }

  "SumGenerator" should "generate AC/BC variants" in {
    val cspom = CSPOM { implicit problem =>
      val v0 = IntVariable(0 to 10)
      val v1 = IntVariable(0 to 10)
      problem.defineBool { r =>
        CSPOMConstraint(r)("sum")(Seq(1, 1), Seq(v0, v1), CSPOMConstant(0)) withParam ("mode" -> "eq")
      } as "r"
    }

    CSPOMCompiler.compile(cspom, Seq(MergeEq)).get

    val (problem, _) = new ProblemGenerator().generate(cspom).get

    withClue(problem.toString(problem.initState.toState)) {
      problem.constraints should have length 3
    }

  }
}