package concrete.constraint.semantic;

import scala.util.Random
import org.junit.Assert.fail
import org.junit.Before
import org.junit.Test
import org.junit.Assert.assertEquals
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import concrete.IntDomain
import concrete.Variable
import concrete.constraint.AdviseCount

final class MulTest {

  private val RAND = new Random
  private var x: Variable = null
  private var y: Variable = null
  private var z: Variable = null

  @Before
  def setUp() {
    x = new Variable("x", IntDomain(randomDomain(-100, 100, 50): _*));
    y = new Variable("y", IntDomain(randomDomain(-100, 100, 50): _*));
    z = new Variable("z", IntDomain(randomDomain(-100, 100, 50): _*));
  }

  @Test
  def testReviseInt() {
    val c = new MulAC(x, y, z);
    c.adviseAll()
    c.revise()

    val c2 = new Constraint(Array(x, y, z)) with Residues with TupleEnumerator {
      def checkValues(t: Array[Int]) = t(0) == t(1) * t(2);
    };
    c2.adviseAll()
    assertEquals(Seq(), c2.revise());
  }

  private def randomDomain(min: Int, max: Int, nb: Int) = {
    var domain: Set[Int] = Set.empty

    while (domain.size < nb) domain += RAND.nextInt(max - min) + min

    domain.toSeq.sorted
  }

}
