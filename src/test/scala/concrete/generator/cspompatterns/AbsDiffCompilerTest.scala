package concrete.generator.cspompatterns

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.CSPOMDriver.CSPOMIntExpressionOperations
import concrete.CSPOMDriver.abs
import cspom.CSPOM
import cspom.compiler.CSPOMCompiler
import cspom.variable.IntVariable
import cspom.CSPOMConstraint

class AbsDiffCompilerTest extends FlatSpec with Matchers {

  "AbsDiff compiler" should "compile" in {

    val cspom = CSPOM { implicit problem =>

      val v0 = IntVariable(1 to 3)
      val v1 = IntVariable(2 to 4)

      val r = v0 - v1

      val r2 = abs(r) as "r2"

    }

    CSPOMCompiler.compile(cspom, Seq(AbsDiff))

    withClue(cspom.toString) {
      cspom.getPostponed shouldBe empty
      cspom.constraints.toSeq should have size 1
      cspom.referencedExpressions should have size 3

      val c = cspom.constraints.next
      c.function shouldEqual 'absdiff
      c.result shouldBe an[IntVariable]
    }
  }
}
