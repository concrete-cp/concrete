package concrete.generator.cspompattern
import cspom.CSPOM
import cspom.CSPOM._
import org.junit.Assert._
import org.junit.Test
import cspom.variable.IntVariable
import concrete.generator.cspompatterns.MergeEq
import cspom.variable.CSPOMVariable
import cspom.variable.BoolVariable

class MergeEqTest {
  @Test
  def testExt() {

    val (cspom, (v0, eq)) = CSPOM withResult {
      val v0 = varOf(1, 2, 3)
      val v1 = CSPOMVariable.ofIntSeq(Seq(2, 3, 4), "var_is_introduced")
      val eq = ctr(v0 === v1) //ctr("eq", v0, v1)
      (v0, eq)
    }

    for (d <- MergeEq.mtch(eq, cspom)) {
      MergeEq.compile(eq, cspom, d)
    }

    assertEquals(1, cspom.variables.size)
    assertSame(v0, cspom.variables.iterator.next)
    assertTrue(cspom.constraints.isEmpty)
    assertEquals(List(2, 3), v0.domain)

  }

  @Test
  def testInt() {

    val (cspom, (v0, eq)) = CSPOM withResult {
      val v0 = interVar(1, 3)
      val v1 = CSPOMVariable.ofIntSeq(Seq(2, 3, 4), "var_is_introduced")
      val eq = ctr(v0 === v1)
      (v0, eq)
    }

    for (d <- MergeEq.mtch(eq, cspom)) {
      MergeEq.compile(eq, cspom, d)
    }

    assertEquals(1, cspom.variables.size)
    assertSame(v0, cspom.variables.iterator.next)
    assertTrue(cspom.constraints.isEmpty)
    assertEquals(List(2, 3), v0.domain)

  }
}