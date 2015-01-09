package concrete.generator.cspompattern

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.generator.cspompatterns.AllDiff
import cspom.CSPOM
import cspom.CSPOM.ctr
import cspom.compiler.MergeEq
import cspom.compiler.ProblemCompiler
import cspom.variable.IntVariable

object AllDiffTest extends FlatSpec with Matchers {

  "AllDiff" should "compile" in {
    val cspom = CSPOM { implicit problem =>

      val v0 = IntVariable.ofSeq(1, 2, 3)
      val v1 = IntVariable.ofSeq(2, 3, 4)
      val v2 = IntVariable.ofSeq(1, 2, 3)
      val v3 = IntVariable.ofSeq(1, 2, 3)

      for (Seq(p1, p2) <- List(v0, v1, v2, v3).combinations(2)) {
        ctr(p1 ≠ p2)
      }
    }

    ProblemCompiler.compile(cspom, Seq(MergeEq, AllDiff))

    //println(cspom)
    cspom.constraints should have size 1
    cspom.constraints.next.function shouldBe 'allDifferent
  }

}
