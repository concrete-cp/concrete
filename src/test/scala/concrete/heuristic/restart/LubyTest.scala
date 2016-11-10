package concrete.heuristic.restart

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.ParameterManager

class LubyTest extends FlatSpec with Matchers {
  "Luby" should "generate test sequence" in {
    val pm = new ParameterManager()
    pm("luby.base") = 1
    
    val luby = new Luby(pm, null)

    Seq.fill(15)(luby.nextRun()) should contain theSameElementsInOrderAs Seq(1, 1, 2, 1, 1, 2, 4, 1, 1, 2, 1, 1, 2, 4, 8)

  }
}