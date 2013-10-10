package concrete.constraint.semantic;

import scala.util.Random
import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.Test
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import concrete.IntDomain
import concrete.Variable
import concrete.AdviseCount

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
    val c = new AbsDiff(x, y, z);
    AdviseCount.adviseAll(c)
    c.revise()

    val c2 = new Constraint(Array(x, y, z)) with Residues with TupleEnumerator {
      def checkValues(t: Array[Int]) = t(0) == math.abs(t(1) - t(2));
    };
    AdviseCount.adviseAll(c2)
    assertEquals(List(), c2.revise());
  }

  @Test
  def testRevise2() {

    val c2 = new Constraint(Array(x, y, z)) with Residues with TupleEnumerator {
      def checkValues(t: Array[Int]) = t(0) == math.abs(t(1) - t(2));
    };
    AdviseCount.adviseAll(c2)
    c2.revise()

    val c = new AbsDiff(x, y, z);
    AdviseCount.adviseAll(c)

    assertEquals(List(), c.revise());
  }

  private def randomDomain(min: Int, max: Int, nb: Int) = {
    var domain: Set[Int] = Set.empty

    while (domain.size < nb) domain += RAND.nextInt(max - min) + min

    domain.toSeq.sorted
  }

}