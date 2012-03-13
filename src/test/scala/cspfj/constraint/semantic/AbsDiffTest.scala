package cspfj.constraint.semantic;

import scala.util.Random

import org.junit.Assert.assertFalse
import org.junit.Before
import org.junit.Test

import cspfj.constraint.AbstractConstraint
import cspfj.constraint.Residues
import cspfj.constraint.TupleEnumerator
import cspfj.problem.IntDomain
import cspfj.problem.Variable
final class AbsDiffTest {

  private val RAND = new Random
  private var x: Variable = null
  private var y: Variable = null
  private var z: Variable = null

  @Before
  def setUp() {
    x = new Variable("x", new IntDomain(randomDomain(-100, 100, 20): _*));
    y = new Variable("y", new IntDomain(randomDomain(-100, 100, 20): _*));
    z = new Variable("z", new IntDomain(randomDomain(-100, 100, 20): _*));
  }

  @Test
  def testReviseInt() {
    val c = new AbsDiff(x, y, z);
    c.fillRemovals()
    c.revise()

    val c2 = new AbstractConstraint(Array(x, y, z)) with Residues with TupleEnumerator {
      def check() = value(0) == math.abs(value(1) - value(2));
      override def getEvaluation = 0
    };
    c2.fillRemovals()
    assertFalse(c2.revise());
  }

  @Test
  def testRevise2() {

    val c2 = new AbstractConstraint(Array(x, y, z)) with Residues with TupleEnumerator {
      def check() = value(0) == math.abs(value(1) - value(2));
      override def getEvaluation = 0
    };
    c2.fillRemovals()
    c2.revise()

    val c = new AbsDiff(x, y, z);
    c.fillRemovals()

    assertFalse(c.revise());
  }

  private def randomDomain(min: Int, max: Int, nb: Int) = {
    var domain: Set[Int] = Set.empty

    while (domain.size < nb) domain += RAND.nextInt(max - min) + min

    domain.toSeq.sorted
  }

}
