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

class DiffGeTest {
  @Test
  def testGen() {

    val (cspom, sub) = CSPOM withResult {

      val r = auxInt()
      assertTrue(r.params("var_is_introduced"))
      ctr(r >= interVar(0, 5))

      ctr(new CSPOMConstraint(r, 'sub, varOf(1, 2, 3), varOf(2, 3, 4)))

    }
    for (d <- DiffGe.mtch.lift(sub, cspom)) {
      DiffGe.compile(sub, cspom, d)
    }

    assertEquals(3, cspom.variables.size)
    assertEquals(1, cspom.constraints.size)
    val c = cspom.constraints.head
    assertEquals('diffGe, c.function)
    assertEquals(CSPOMTrue, c.result)
  }

  @Test
  def testFunc() {

    val (cspom, sub) = CSPOM withResult {

      val r = auxInt()

      val r2 = (r >= interVar(0, 5))

      ctr(new CSPOMConstraint(r, 'sub, varOf(1, 2, 3), varOf(2, 3, 4)))
    }
    for (d <- DiffGe.mtch.lift(sub, cspom)) {
      DiffGe.compile(sub, cspom, d)
    }
    assertEquals(4, cspom.variables.size)
    assertEquals(1, cspom.constraints.size)
    val c = cspom.constraints.head
    assertEquals('diffGe, c.function)
    assertTrue(c.result.isInstanceOf[BoolVariable])
  }

}