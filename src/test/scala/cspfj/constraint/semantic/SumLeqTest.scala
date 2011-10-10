package cspfj.constraint.semantic;

import org.junit.Assert.assertArrayEquals;
import org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.Constraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Variable;

final class SumLeqTest {

  private var v0: Variable = null;
  private var v1: Variable = null;
  private var c: Constraint = null;

  @Before
  def setUp() {
    v0 = new Variable("v0", new BitVectorDomain(1, 2, 3, 4));
    v1 = new Variable("v1", new BitVectorDomain(0, 1, 2, 3, 4));
    c = new SumLeq(4, v0, v1);
  }

  @Test
  def reviseTest() {
    assertTrue(c.revise(new RevisionHandler() {
      def revised(constraint: Constraint, variable: Variable) {
      }
    }, 0));

    assertArrayEquals(Array(1, 2, 3, 4), v0.domain.currentValues);
    assertArrayEquals(Array(0, 1, 2, 3), v1.domain.currentValues);
  }

}