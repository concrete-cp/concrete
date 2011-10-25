package cspfj.constraint.semantic;

import org.junit.Assert.assertEquals;
import org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.Constraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

final class InIntervalTest {

  private var domain: Domain = null;
  private var constraint: InInterval = null;

  @Before
  def setUp() {
    domain = new BitVectorDomain(1, 2, 3)
    val variable = new Variable("test", domain);
    constraint = InInterval.values(variable, 2, 4);
  }

  @Test
  def testIsConsistent() {
    assertTrue(constraint.isConsistent(0));
  }

  @Test
  def testRevise() {
    assertTrue(constraint.revise(new RevisionHandler() {
      def revised(constraint: Constraint,
        variable: Variable) {
      }
    }, 0));
    assertEquals(Seq(2, 3), domain.values.toSeq);
  }

}
