package concrete.generator.cspompatterns

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import cspom.CSPOM
import cspom.variable.IntVariable
import concrete.CSPOMDriver._
import CSPOM._
import cspom.compiler.CSPOMCompiler
import cspom.compiler.MergeEq
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq

class SumDuplicatesTest extends FlatSpec with Matchers {
  "SumDuplicates" should "merge" in {
    val problem: CSPOM = CSPOM { implicit p =>
      val Seq(x, y) = Seq.fill(2) { IntVariable.free() }

      ctr(linear(Seq((4, x), (-4, y), (2, x)), "le", -2))
    }

    CSPOMCompiler.compile(problem, Seq(SumDuplicates, MergeEq))

    withClue(problem) {
      val Seq(constraint) = problem.constraints.toSeq

      val Seq(CSPOMSeq(args), CSPOMConstant(const)) = constraint.arguments

      args should have size 2
    }

  }
}