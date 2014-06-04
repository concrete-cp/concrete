package concrete.generator.cspompattern
import cspom.CSPOM
import cspom.CSPOM._
import concrete.CSPOMDriver._
import org.junit.Assert._
import org.junit.Test
import cspom.CSPOMConstraint
import concrete.generator.cspompatterns.AbsDiff
import cspom.variable.BoolVariable
import cspom.variable.IntVariable
import cspom.compiler.ProblemCompiler
import org.scalatest.Matchers
import org.scalatest.FlatSpec

object AbsDiffTest extends FlatSpec with Matchers {

  "AbsDiff compiler" should "compile" in {

    val cspom = CSPOM { implicit problem =>

      val v0 = IntVariable(1 to 3)
      val v1 = IntVariable(2 to 4)

      val r = v0 - v1

      assertTrue(r.hasParam("var_is_introduced"))

      val r2 = abs(r)

    }

    ProblemCompiler.compile(cspom, Seq(AbsDiff))

    cspom.referencedExpressions should have size 3
    cspom.constraints should have size 1

    val c = cspom.constraints.next
    c.function shouldBe 'absdiff
    c.result shouldBe an[IntVariable]
  }
}
