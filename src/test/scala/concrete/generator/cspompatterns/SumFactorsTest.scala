package concrete.generator.cspompatterns

import concrete.CSPOMDriver._
import concrete.generator.SumGenerator
import cspom.CSPOM
import cspom.CSPOM._
import cspom.compiler.{CSPOMCompiler, MergeEq}
import cspom.variable.IntVariable
import org.scalatest.{FlatSpec, Matchers}

class SumFactorsTest extends FlatSpec with Matchers {
  "SumFactors" should "canonize" in {
    val problem = CSPOM { implicit p =>
      val Seq(x, y, z) = Seq.fill(3) {
        IntVariable.free()
      }

      ctr(4 *: x + -4 *: y + 2 *: z <= -2)
    }

    CSPOMCompiler.compile(problem, Seq(SumFactors, MergeEq))
    val Seq(constraint) = problem.constraints.toSeq

    val (vars, coefs, constant, mode) = SumGenerator.readCSPOM(constraint)

    coefs shouldBe Seq(2, -2, 1)
    constant shouldBe -1
  }
}