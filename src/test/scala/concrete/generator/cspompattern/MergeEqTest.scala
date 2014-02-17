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

    val (cspom, (v0, eq)) = CSPOM withResult {
      val v0 = varOf(1, 2, 3) as "V0"
      ctr(new CSPOMConstraint('dummy, v0))
      val v1 = varOfSeq(Seq(2, 3, 4), "var_is_introduced")
      val eq = ctr(v0 === v1) //ctr("eq", v0, v1)
      (v0, eq)
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

  @Test
  def testInt() {

    val (cspom, (v0, eq)) = CSPOM withResult {
      val v0 = interVar(1, 3) as "V0"
      ctr(new CSPOMConstraint('dummy, v0))
      val v1 = varOfSeq(Seq(2, 3, 4), "var_is_introduced")
      val eq = ctr(v0 === v1)
      (v0, eq)
    }

    ProblemCompiler.compile(cspom, Seq(MergeEq))

    assertEquals(1, cspom.namedExpressions.size)
    val nv0 = cspom.expression("V0") collect {
      case v: IntVariable => v
    } getOrElse {
      throw new AssertionFailedError()
    }
    assertEquals(1, cspom.constraints.size)
    assertSame(nv0, cspom.namedExpressions.keySet.head)
    assertEquals(List(2, 3), nv0.domain)

  }
}