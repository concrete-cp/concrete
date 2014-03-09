package concrete.generator.cspompattern
import cspom.CSPOM
import cspom.CSPOM._
import org.junit.Assert._
import org.junit.Test
import cspom.variable.IntVariable
import cspom.compiler.MergeEq
import cspom.variable.CSPOMVariable
import cspom.variable.BoolVariable
import concrete.CSPOMDriver._
import cspom.CSPOMConstraint
import junit.framework.AssertionFailedError
import cspom.compiler.ProblemCompiler

class MergeEqTest {
  @Test
  def testExt() {

    val cspom = CSPOM {
      val v0 = IntVariable(1 to 3) as "V0"
      ctr(CSPOMConstraint('dummy, v0))
      val v1 = IntVariable(2 to 4, Set("var_is_introduced"))
      ctr(v0 === v1)
    }

    ProblemCompiler.compile(cspom, Seq(MergeEq))

    val nv0 = cspom.expression("V0") collect {
      case v: IntVariable => v
    } getOrElse {
      throw new AssertionFailedError()
    }
    assertEquals(1, cspom.namedExpressions.size)
    assertSame(nv0, cspom.namedExpressions.head._2)
    assertEquals(cspom.toString, 1, cspom.constraints.size)
    assertEquals(List(2, 3), nv0.domain)

  }

}