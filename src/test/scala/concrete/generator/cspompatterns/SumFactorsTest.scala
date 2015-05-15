package concrete.generator.cspompatterns

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import cspom.CSPOM
import cspom.variable.IntVariable
import concrete.CSPOMDriver._
import CSPOM._
import cspom.compiler.CSPOMCompiler

class SumFactorsTest extends FlatSpec with Matchers {
  "SumFactors" should "canonize" in {
    val problem = CSPOM { implicit p =>
      val Seq(x, y, z) = Seq.fill(3) { IntVariable.free() }

      ctr(linear(Seq((4, x), (-4, y), (2, z)), "le", -2))
    }

    CSPOMCompiler.compile(problem, Seq(SumFactors))
    println(problem)
  }
}