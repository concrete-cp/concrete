package concrete.generator.cspompattern

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.CSPOMDriver.CSPOMIntExpressionOperations
import concrete.CSPOMDriver.abs
import concrete.generator.cspompatterns.AbsDiff
import concrete.generator.cspompatterns.SubToAdd
import cspom.CSPOM
import cspom.compiler.ProblemCompiler
import cspom.variable.IntVariable

class AbsDiffTest extends FlatSpec with Matchers {

  "AbsDiff compiler" should "compile" in {

    val cspom = CSPOM { implicit problem =>

      val v0 = IntVariable(1 to 3)
      val v1 = IntVariable(2 to 4)

      val r = v0 - v1

      assert(problem.namesOf(r).isEmpty)

      val r2 = abs(r)

    }

    ProblemCompiler.compile(cspom, Seq(SubToAdd, AbsDiff))

    cspom.constraints.toSeq should have size 1
    cspom.referencedExpressions should have size 3

    val c = cspom.constraints.next
    c.function shouldEqual 'absdiff
    c.result shouldBe an[IntVariable]
  }
}
