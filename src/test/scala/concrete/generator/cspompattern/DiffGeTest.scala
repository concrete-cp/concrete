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

class DiffGeTest {
  @Test
  def testGen() {

    val cspom = CSPOM {

      val r = IntVariable(1 to 3) - IntVariable(2 to 4)
      assertTrue(r.hasParam("var_is_introduced"))
      ctr(r >= IntVariable(0 to 5))

    }

    ProblemCompiler.compile(cspom, Seq(MergeEq, DiffGe))

    assertEquals(cspom.toString, 4, cspom.referencedExpressions.size)
    assertEquals(1, cspom.constraints.size)
    val c = cspom.constraints.head
    assertEquals('diffGe, c.function)
    assertEquals(CSPOMConstant(true), c.result)
  }

  @Test
  def testFunc() {

    val cspom = CSPOM {

      val r = IntVariable(1 to 3) - IntVariable(2 to 4)

      val r2 = (r >= IntVariable(0 to 5))

    }

    ProblemCompiler.compile(cspom, Seq(DiffGe))

    assertEquals(4, cspom.referencedExpressions.size)
    assertEquals(1, cspom.constraints.size)
    val c = cspom.constraints.head
    assertEquals('diffGe, c.function)
    assertTrue(c.result.isInstanceOf[BoolVariable])
  }

}
