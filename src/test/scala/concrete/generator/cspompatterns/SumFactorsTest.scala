package concrete.generator.cspompatterns

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.CSPOMDriver._
import cspom.CSPOM
import cspom.CSPOM._
import cspom.variable.IntVariable
import cspom.compiler.MergeEq
import cspom.compiler.CSPOMCompiler
import cspom.variable.CSPOMConstant
import concrete.generator.SumGenerator

class SumFactorsTest extends FlatSpec with Matchers {
  "SumFactors" should "canonize" in {
    val problem = CSPOM { implicit p =>
      val Seq(x, y, z) = Seq.fill(3) { IntVariable.free() }

      ctr(linear(Seq((4, x), (-4, y), (2, z)), "le", -2))
    }

    CSPOMCompiler.compile(problem, Seq(SumFactors, MergeEq))
    val Seq(constraint) = problem.constraints.toSeq

    val (vars, coefs, constant, mode) = SumGenerator.readCSPOM(constraint)

    coefs shouldBe Seq(2, -2, 1)
    constant shouldBe -1
  }
}