package concrete.generator.cspompattern
import cspom.CSPOM
import cspom.CSPOM._
import org.junit.Assert._
import org.junit.Test

class MergeEqTest {
  @Test
  def testExt() {

    val (cspom, (v0, eq)) = CSPOM withResult {
      val v0 = varOf(1, 2, 3)
      val v1 = aux()
      v1.domain = ExtensiveDomain.of(2, 3, 4)
      assertTrue(v1.auxiliary)
      val eq = ctr(v0 == v1) //ctr("eq", v0, v1)
      (v0, eq)
    }

    new MergeEq(cspom, new Queue[CSPOMConstraint]).compile(eq);

    assertEquals(1, cspom.variables.size)
    assertSame(v0, cspom.variables.iterator.next)
    assertTrue(cspom.constraints.isEmpty)
    assertEquals(List(2, 3), v0.domain.values)

  }

  @Test
  def testInt() {

    val (cspom, (v0, eq)) = CSPOM withResult {
      val v0 = interVar(1, 3)
      val v1 = aux()
      v1.domain = ExtensiveDomain.of(2, 3, 4)
      assertTrue(v1.auxiliary)
      val eq = ctr(v0 == v1)
      (v0, eq)
    }

    new MergeEq(cspom, new Queue[CSPOMConstraint]).compile(eq);

    assertEquals(1, cspom.variables.size)
    assertSame(v0, cspom.variables.iterator.next)
    assertTrue(cspom.constraints.isEmpty)
    assertEquals(List(2, 3), v0.domain.values)

  }
}