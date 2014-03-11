package concrete.generator.cspompattern
import cspom.CSPOM
import cspom.CSPOM._
import org.junit.Assert._
import org.junit.Test
import cspom.CSPOMConstraint
import concrete.generator.cspompatterns.DiffGe
import cspom.variable.CSPOMTrue
import cspom.variable.BoolVariable
import org.hamcrest.CoreMatchers._
import concrete.CSPOMDriver._
import cspom.compiler.ProblemCompiler
import cspom.compiler.MergeEq
import cspom.variable.IntVariable

class DiffGeTest {
  @Test
  def testGen() {

    val (cspom, sub) = CSPOM withResult {

      val r = auxInt()
      assertTrue(r.hasParam("var_is_introduced"))
      ctr(r >= IntVariable.ofInterval(0, 5))

      ctr(CSPOMConstraint(r, 'sub, IntVariable.of(1, 2, 3), IntVariable.of(2, 3, 4)))

    }

    ProblemCompiler.compile(cspom, Seq(MergeEq, DiffGe))

    assertEquals(cspom.toString, 4, cspom.referencedExpressions.size)
    assertEquals(1, cspom.constraints.size)
    val c = cspom.constraints.head
    assertEquals('diffGe, c.function)
    assertEquals(CSPOMTrue, c.result)
  }

  @Test
  def testFunc() {

    val (cspom, sub) = CSPOM withResult {

      val r = auxInt()

      val r2 = (r >= IntVariable.ofInterval(0, 5))

      ctr(CSPOMConstraint(r, 'sub, IntVariable.of(1, 2, 3), IntVariable.of(2, 3, 4)))
    }
    for (d <- DiffGe.mtch(sub, cspom)) {
      DiffGe.compile(sub, cspom, d)
    }
    assertEquals(4, cspom.referencedExpressions.size)
    assertEquals(1, cspom.constraints.size)
    val c = cspom.constraints.head
    assertEquals('diffGe, c.function)
    assertTrue(c.result.isInstanceOf[BoolVariable])
  }

}