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


class AbsDiffTest {
  @Test
  def testExt() {
    val cspom = CSPOM {

      val v0 = IntVariable(Seq(1, 2, 3))
      val v1 = IntVariable(Seq(2, 3, 4))

      val r = v0 - v1

      assertTrue(r.hasParam("var_is_introduced"))

      val r2 = abs(r)

    }

    ProblemCompiler.compile(cspom, Seq(AbsDiff))

    assertEquals(3, cspom.referencedExpressions.size)
    assertEquals(1, cspom.constraints.size)

    val c = cspom.constraints.head
    assertEquals('absdiff, c.function)
    assertFalse(c.result.isInstanceOf[BoolVariable])
  }

}
