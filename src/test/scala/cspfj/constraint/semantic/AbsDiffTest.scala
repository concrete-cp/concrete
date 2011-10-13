package cspfj.constraint.semantic;

import org.junit.Assert.fail
import org.junit.Before
import org.junit.Test
import cspfj.constraint.AbstractConstraint
import cspfj.constraint.Constraint
import cspfj.filter.RevisionHandler
import cspfj.problem.BitVectorDomain
import cspfj.problem.Variable
import scala.util.Random
import scala.collection.immutable.SortedSet
import cspfj.constraint.Residues
import cspfj.constraint.TupleEnumerator

final class AbsDiffTest {

  private val RAND = new Random
  private var x: Variable = null
  private var y: Variable = null
  private var z: Variable = null

  @Before
  def setUp() {
    x = new Variable("x", new BitVectorDomain(randomDomain(-100, 100, 20): _*));
    y = new Variable("y", new BitVectorDomain(randomDomain(-100, 100, 20): _*));
    z = new Variable("z", new BitVectorDomain(randomDomain(-100, 100, 20): _*));
  }

  @Test
  def testReviseInt() {
    val c = new AbsDiff(x, y, z);
    c.revise(new RevisionHandler() {
      def revised(constraint: Constraint, variable: Variable) {}
    }, 0);

    val c2 = new AbstractConstraint(Array(x, y, z)) with Residues with TupleEnumerator {
      def check() = value(0) == math.abs(value(1) - value(2));
      override def getEvaluation = 0
    };
    c2.revise(new RevisionHandler() {
      def revised(constraint: Constraint, variable: Variable) {
        fail("Revised " + variable);
      }
    }, 0);
  }

  private def randomDomain(min: Int, max: Int, nb: Int) = {
    var domain: SortedSet[Int] = SortedSet.empty

    while (domain.size < nb) domain += RAND.nextInt(max - min) + min

    domain.toSeq
  }

}
