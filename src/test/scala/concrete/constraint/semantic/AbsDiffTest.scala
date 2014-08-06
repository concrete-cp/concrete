package concrete.constraint.semantic;

import scala.util.Random

import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.Test

import concrete.IntDomain
import concrete.Variable
import concrete.constraint.AdviseCount
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator

final class AbsDiffTest {

  private val RAND = new Random
  private var x: Variable = null
  private var y: Variable = null
  private var z: Variable = null

  @Before
  def setUp() {
    x = new Variable("x", IntDomain(randomDomain(-100, 100, 20): _*));
    y = new Variable("y", IntDomain(randomDomain(-100, 100, 20): _*));
    z = new Variable("z", IntDomain(randomDomain(-100, 100, 20): _*));
  }

  @Test
  def testReviseInt() {
    val c = new AbsDiffAC(x, y, z);
    c.register(new AdviseCount)
    c.adviseAll()
    c.revise()

    val c2 = new Constraint(Array(x, y, z)) with Residues with TupleEnumerator {
      def checkValues(t: Array[Int]) = t(0) == math.abs(t(1) - t(2));
    };
    c2.register(new AdviseCount)
    c2.adviseAll()
    assertEquals(List(), c2.revise());
  }

  @Test
  def testRevise2() {

    val c2 = new Constraint(Array(x, y, z)) with Residues with TupleEnumerator {
      def checkValues(t: Array[Int]) = t(0) == math.abs(t(1) - t(2));
    };
    c2.register(new AdviseCount)
    c2.adviseAll()
    c2.revise()

    val c = new AbsDiffAC(x, y, z);
    c.register(new AdviseCount)
    c.adviseAll()

    assertEquals(List(), c.revise());
  }

  private def randomDomain(min: Int, max: Int, nb: Int) = {
    var domain: Set[Int] = Set.empty

    while (domain.size < nb) domain += RAND.nextInt(max - min) + min

    domain.toSeq.sorted
  }

  @Test
  def testRevise3() {
    val x = new Variable("x", IntDomain(0, 2, 3))
    val y = new Variable("y", IntDomain(3, 4, 6, 7, 8))
    val z = new Variable("z", IntDomain(5))

    val c = new AbsDiffAC(x, y, z)
    c.register(new AdviseCount)
    c.adviseAll()
    c.revise()

    assertEquals(2, x.dom.size)
    assertEquals(3, y.dom.size)
  }

}
