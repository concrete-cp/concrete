package concrete.generator.constraint

import concrete.generator.ProblemGenerator
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.CSPOM._
import cspom.variable.IntVariable
import concrete.constraint.semantic.AbsDiffAC
import concrete.constraint.semantic.AbsDiffBC
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class AbsDiffGeneratorTest extends AnyFlatSpec with Matchers with TryValues {

  "AbsDiffGenerator" should "generate constraints" in {

    val cspom = CSPOM { implicit problem =>

      val v0 = IntVariable(1, 2, 3)
      val v1 = IntVariable(1, 2, 3)
      val v2 = IntVariable(1, 2, 3)

      ctr(CSPOMConstraint(v0, "absdiff", Seq(v1, v2)))
    }

    val problem = new ProblemGenerator().generate(cspom).success.value._1

    problem.constraints.map(_.getClass()).toSet shouldBe
      Set(classOf[AbsDiffBC], classOf[AbsDiffAC])

  }
}
