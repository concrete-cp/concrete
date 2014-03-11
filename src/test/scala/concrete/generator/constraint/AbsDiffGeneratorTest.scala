package concrete.generator.constraint

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import concrete.constraint.semantic.AbsDiff
import concrete.generator.ProblemGenerator
import concrete.Problem
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.CSPOM._
import cspom.variable.IntVariable

final class AbsDiffGeneratorTest {

  @Test
  def test() {

    val cspom = CSPOM {

      val v0 = IntVariable(Seq(1, 2, 3))
      val v1 = IntVariable(Seq(1, 2, 3))
      val v2 = IntVariable(Seq(1, 2, 3))

      ctr(CSPOMConstraint(v0, 'absdiff, v1, v2))
    }

    val problem = ProblemGenerator.generate(cspom)

    assertEquals(1, problem.constraints.size)
    assertTrue(problem.constraints.head.isInstanceOf[AbsDiff])

  }
}
