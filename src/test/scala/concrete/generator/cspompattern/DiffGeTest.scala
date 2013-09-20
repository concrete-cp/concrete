package concrete.generator.cspompattern
import cspom.CSPOM
import cspom.CSPOM._
import org.junit.Assert._
import org.junit.Test

class DiffGeTest {
  @Test
  def testGen() {

    val (cspom, sub) = CSPOM withResult {

      val r = aux()
      assertTrue(r.auxiliary)
      ctr(r >= interVar(0, 5))

      threadLocalProblem.addConstraint(new FunctionalConstraint(r, "sub", varOf(1, 2, 3), varOf(2, 3, 4)))

    }
    new DiffGe(cspom).compile(sub)
    //println(cspom)
    assertEquals(3, cspom.variables.size)
    assertEquals(1, cspom.constraints.size)
    assertEquals("diffGe", cspom.constraints.iterator.next.description)
    assertTrue(cspom.constraints.iterator.next.isInstanceOf[GeneralConstraint])
  }

  @Test
  def testFunc() {

    val (cspom, sub) = CSPOM withResult {

      val r = aux()

      val r2 = (r >= interVar(0, 5))

      threadLocalProblem.addConstraint(new FunctionalConstraint(r, "sub", varOf(1, 2, 3), varOf(2, 3, 4)))
    }
    println(cspom)
    new DiffGe(cspom).compile(sub)
    println(cspom)
    assertEquals(4, cspom.variables.size)
    assertEquals(1, cspom.constraints.size)
    assertEquals("diffGe", cspom.constraints.iterator.next.description)
    assertTrue(cspom.constraints.iterator.next.isInstanceOf[FunctionalConstraint])
  }

}