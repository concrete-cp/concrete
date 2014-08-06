package concrete.generator.cspompattern

import cspom.variable.CSPOMSeq
import org.junit.Test
import cspom.variable.IntVariable
import concrete.generator.cspompatterns.Element
import org.junit.Assert._

class ElementTest {
  @Test
  def test(): Unit = {
    val as = IndexedSeq(
      IntVariable(1 to 3),
      IntVariable(3 to 4),
      IntVariable(5 to 7))

    val b = IntVariable(1 to 3)

    val c = IntVariable(1 to 7)

    val mdd = Element.elementVar(as.map(_.toSeq), 2 to 4)

    //mdd.foreach(println)
    assertEquals(54, mdd.size)

  }
}