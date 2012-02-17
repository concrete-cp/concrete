package cspfj.constraint.semantic
import org.junit.Test

import cspfj.problem.BitVectorDomain
import cspfj.problem.Variable

class AllDiffTest {
  @Test
  def test() {
    val v1 = new Variable("1", new BitVectorDomain(7))
    val v2 = new Variable("2", new BitVectorDomain(6))
    val v3 = new Variable("3", new BitVectorDomain(7, 9))
    val v4 = new Variable("4", new BitVectorDomain(8))
    val v5 = new Variable("5", new BitVectorDomain(8, 9))

    val c = new AllDifferent(v1, v2, v3, v4, v5)

    c.revise()
  }
}