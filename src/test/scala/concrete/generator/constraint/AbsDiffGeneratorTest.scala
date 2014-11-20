package concrete.generator.constraint

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import concrete.generator.ProblemGenerator
import concrete.Problem
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.CSPOM._
import cspom.variable.IntVariable
import concrete.constraint.semantic.AbsDiffAC
import concrete.constraint.semantic.AbsDiffBC

final class AbsDiffGeneratorTest {

  @Test
  def test() {

    val cspom = CSPOM { implicit problem =>

      val v0 = IntVariable.ofSeq(1, 2, 3)
      val v1 = IntVariable.ofSeq(1, 2, 3)
      val v2 = IntVariable.ofSeq(1, 2, 3)

      ctr(CSPOMConstraint(v0, 'absdiff, Seq(v1, v2)))
    }

    val problem = new ProblemGenerator().generate(cspom)._1

    assertEquals(2, problem.constraints.size)
    assertEquals(Set(classOf[AbsDiffBC], classOf[AbsDiffAC]), problem.constraints.map(_.getClass()).toSet)

  }
}
