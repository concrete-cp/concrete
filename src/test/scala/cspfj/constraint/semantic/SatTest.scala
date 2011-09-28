package cspfj.constraint.semantic
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

import cspfj.constraint.Sat
import cspfj.problem.BitVectorDomain
import cspfj.problem.Problem

class SatTest {
  @Test
  def test {
    val problem = new Problem
    val v0 = problem.addVariable("v0", new BitVectorDomain(0, 1, 2));
    val v1 = problem.addVariable("v1", new BitVectorDomain(0, 1, 2));
    val v2 = problem.addVariable("v2", new BitVectorDomain(0, 1, 2));

    val c = new Sat(v0, v1, v2)
    problem.addConstraint(c);

    assertTrue(c.check)
    c.noGood(Seq((v0, 0), (v1, 1)))
    c.getTuple(1) = 1
    assertFalse(c.check)

    v1.remove(0)
    v1.remove(2)
    assertFalse(c.findSupport(0, 0))
    
    assertTrue(c.findSupport(0, 1))
  }
}