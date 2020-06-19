package concrete.generator.cspompatterns

import concrete.CSPOMDriver._
import cspom.CSPOM
import cspom.CSPOM._
import cspom.compiler.{CSPOMCompiler, MergeEq}
import cspom.variable.{CSPOMConstant, CSPOMSeq, IntExpression, IntVariable}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SumDuplicatesTest extends AnyFlatSpec with Matchers {
  "SumDuplicates" should "merge" in {
    val problem: CSPOM = CSPOM { implicit p =>
      val Seq(x, y) = Seq.fill(2) {
        IntVariable.free()
      }

      ctr(4 *: x + -4 *: y + 2 *: x <= -2)
    }

    CSPOMCompiler.compile(problem, Seq(SumDuplicates, MergeEq))

    withClue(problem) {
      val Seq(constraint) = problem.constraints.toSeq

      val Seq(IntExpression.constSeq(coefs), CSPOMSeq(args), CSPOMConstant(const)) = constraint.arguments

      coefs should have size 2
      args should have size 2
    }

  }
}