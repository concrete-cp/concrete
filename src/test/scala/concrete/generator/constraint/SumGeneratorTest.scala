package concrete.generator.constraint

import org.junit.Test
import cspom.CSPOM
import concrete.CSPOMDriver._
import cspom.variable.IntVariable
import concrete.generator.ProblemGenerator
import cspom.variable.CSPOMConstant
import org.junit.Assert.assertEquals
import concrete.util.Interval
import cspom.CSPOM.ctr
import cspom.compiler.ProblemCompiler
import cspom.compiler.MergeEq

class SumGeneratorTest {

  @Test
  def test(): Unit = {

    val cspom = CSPOM { implicit problem =>
      val r = sumProd((1, IntVariable(0 to 5)), (2, IntVariable(10 to 15)), (3, CSPOMConstant(20))) as "test"
    }

    var problem = ProblemGenerator.generate(cspom)._1

    assertEquals(Interval(80, 95), problem.variable("test").dom.valueInterval)

  }

  @Test
  def test2(): Unit = {

    val cspom = CSPOM { implicit problem =>
      ctr(sumProd((1, IntVariable(0 to 5)), (2, IntVariable.free() as "test"), (3, CSPOMConstant(20))) === CSPOMConstant(5))
    }

    ProblemCompiler.compile(cspom, Seq(MergeEq))

    var problem = ProblemGenerator.generate(cspom)._1

    assertEquals(problem.toString, Interval(-30, -28), problem.variable("test").dom.valueInterval)

  }
}