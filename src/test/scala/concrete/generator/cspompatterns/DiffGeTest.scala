package concrete.generator.cspompattern

import cspom.CSPOM
import cspom.CSPOM._
import org.junit.Assert._
import org.junit.Test
import cspom.CSPOMConstraint
import concrete.generator.cspompatterns.DiffGe
import cspom.variable.CSPOMConstant
import cspom.variable.BoolVariable
import org.hamcrest.CoreMatchers._
import concrete.CSPOMDriver._
import cspom.compiler.ProblemCompiler
import cspom.compiler.MergeEq
import cspom.variable.IntVariable
import org.scalatest.Matchers
import org.scalatest.FlatSpec

class DiffGeTest extends FlatSpec with Matchers {

  "DiffGe" should "compile general" in {

    val cspom = CSPOM { implicit problem =>

      val r = IntVariable(1 to 3) - IntVariable(2 to 4)
      assertTrue(r.hasParam("var_is_introduced"))
      ctr(r >= IntVariable(0 to 5))

    }

    ProblemCompiler.compile(cspom, Seq(MergeEq, DiffGe))

    cspom.referencedExpressions should have size 4
    cspom.constraints should have size 1

    val c = cspom.constraints.next

    c.function shouldEqual 'diffGe

    c.result match {
      case CSPOMConstant(true) =>
      case _ => fail()
    }

  }

  it should "compile functional" in {
    val cspom = CSPOM { implicit problem =>

      val r = IntVariable(1 to 3) - IntVariable(2 to 4)

      val r2 = (r >= IntVariable(0 to 5))

    }

    ProblemCompiler.compile(cspom, Seq(DiffGe))

    cspom.referencedExpressions should have size 4
    cspom.constraints should have size 1
    val c = cspom.constraints.next
    c.function shouldEqual 'diffGe
    c.result shouldBe a [BoolVariable]
  }

}
