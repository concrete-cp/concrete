package concrete.generator.cspompatterns

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.CSPOMDriver.CSPOMIntExpressionOperations
import concrete.CSPOMDriver.abs
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.CSPOMCompiler
import cspom.variable.IntVariable

class AbsDiffCompilerTest extends FlatSpec with Matchers {

  "AbsDiff compiler" should "compile CSPOM DSL" in {

    val cspom = CSPOM { implicit problem =>

      val v0 = IntVariable(1 to 3) as "v0"
      val v1 = IntVariable(2 to 4) as "v1"

      val r = v0 - v1

      abs(r) as "r2"

    }

    CSPOMCompiler.compile(cspom, Seq(AbsDiff)).get

    withClue(cspom.toString) {
      cspom.getPostponed shouldBe empty
      cspom.constraints.toSeq should have size 1
      cspom.referencedExpressions should have size 3

      val v0 = cspom.variable("v0").get
      val v1 = cspom.variable("v1").get
      val r2 = cspom.variable("r2").get

      val Seq(CSPOMConstraint(`r2`, "absdiff", Seq(`v0`, `v1`), _)) = cspom.constraints.toSeq

      // println(cspom)
    }
  }

}
