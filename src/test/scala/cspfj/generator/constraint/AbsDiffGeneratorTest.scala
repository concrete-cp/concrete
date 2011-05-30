package cspfj.generator.constraint

import cspfj.problem.Problem
import cspom.constraint.FunctionalConstraint
import cspom.CSPOM
import org.junit.Test

final class AbsDiffGeneratorTest {

  @Test
  def test() {

    val cspom = new CSPOM

    val v0 = cspom.varOf(1, 2, 3)
    val v1 = cspom.varOf(1, 2, 3)
    val v2 = cspom.varOf(1, 2, 3)
    val c = cspom.addConstraint(new FunctionalConstraint(v0, "absdiff", v1, v2))

    val problem = new Problem
    new AbsDiffGenerator(problem).generate(c)

    println(problem)
  }
}