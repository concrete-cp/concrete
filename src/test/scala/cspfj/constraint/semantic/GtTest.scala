package cspfj.constraint.semantic;

import org.junit.Assert.assertArrayEquals;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.Constraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Variable;

final class GtTest {
  var v1: Variable = null
  var v2: Variable = null

  @Before
  def setUp() {
    v1 = new Variable("v1", new BitVectorDomain(1, 2, 3, 4));
    v2 = new Variable("v2", new BitVectorDomain(3, 4, 5));
  }

  @Test
  def testRevise1() {

    val c = new Gt(v1, v2, true);
    c.revise(new RevisionHandler() {
      def revised(c: Constraint, v: Variable) {}
    }, 0);

    assertArrayEquals(Array(4), v1.getDomain.currentValues);
    assertArrayEquals(Array(3), v2.getDomain.currentValues);

  }

  @Test
  def testRevise2() {
    val c = new Gt(v1, v2, false);
    c.revise(new RevisionHandler() {
      def revised(c: Constraint, v: Variable) {}
    }, 0);

    assertArrayEquals(Array(3, 4), v1.getDomain().currentValues);
    assertArrayEquals(Array(3, 4), v2.getDomain().currentValues);

  }

}
