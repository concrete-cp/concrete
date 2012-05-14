package cspfj.generator.constraint

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

import cspfj.constraint.semantic.AbsDiff
import cspfj.generator.ProblemGenerator
import cspfj.Problem
import cspom.constraint.FunctionalConstraint
import cspom.CSPOM

final class AbsDiffGeneratorTest {

  @Test
  def test() {

    val cspom = new CSPOM

    val v0 = cspom.varOf(1, 2, 3)
    val v1 = cspom.varOf(1, 2, 3)
    val v2 = cspom.varOf(1, 2, 3)
    val c = cspom.addConstraint(new FunctionalConstraint(v0, "absdiff", v1, v2))

    val problem = new Problem
    ProblemGenerator.generateVariables(problem, cspom)
    new AbsDiffGenerator(problem).generate(c)

    assertEquals(1, problem.constraints.size)
    assertTrue(problem.constraints.head.isInstanceOf[AbsDiff])

  }
}