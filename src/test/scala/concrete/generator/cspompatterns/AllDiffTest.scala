package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOM._
import cspom.compiler.CSPOMCompiler
import cspom.compiler.MergeEq
import cspom.variable.IntVariable
import concrete.CSPOMDriver._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AllDiffTest extends AnyFlatSpec with Matchers {

  "AllDiff" should "compile" in {
    val cspom = CSPOM { implicit problem =>

      val v0 = IntVariable(1, 2, 3) as "v0"
      val v1 = IntVariable(2, 3, 4) as "v1"
      val v2 = IntVariable(1, 2, 3) as "v2"
      val v3 = IntVariable(1, 2, 3) as "v3"

      for (Seq(p1, p2) <- List(v0, v1, v2, v3).combinations(2)) {
        ctr(!(p1 === p2))
      }
    }

    CSPOMCompiler.compile(cspom, Seq(MergeEq, NegToCNF, SimplClause, UnaryClause, Reversed, AllDiff))

    //println(cspom)
    withClue(cspom) {
      //cspom.constraints should have size 1
      assert(cspom.constraints.toSeq.exists {
        c => c.function == "alldifferent"
      })
    }
  }

  it should "convert x - y != 0 constraint" in {

  }


}
