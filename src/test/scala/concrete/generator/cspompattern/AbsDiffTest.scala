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

class AbsDiffTest {
  @Test
  def testExt() {
    val (cspom, sub) = CSPOM withResult {

      val v0 = IntVariable.of(1, 2, 3)
      val v1 = IntVariable.of(2, 3, 4)
      val r = auxInt()
      assertTrue(r.hasParam("var_is_introduced"))

      val r2 = abs(r)

      ctr(CSPOMConstraint(r, 'sub, v0, v1))
    }

    for (d <- AbsDiff.mtch(sub, cspom)) {
      AbsDiff.compile(sub, cspom, d)
    }

    assertEquals(3, cspom.referencedExpressions.size)
    assertEquals(1, cspom.constraints.size)

    val c = cspom.constraints.head
    assertEquals('absdiff, c.function)
    assertFalse(c.result.isInstanceOf[BoolVariable])
  }

}
