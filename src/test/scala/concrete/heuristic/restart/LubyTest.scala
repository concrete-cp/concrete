package concrete.heuristic.restart

import org.scalatest.{FlatSpec, Matchers, OptionValues}
import concrete.ParameterManager

class LubyTest extends FlatSpec with Matchers with OptionValues {
  "Luby" should "generate test sequence" in {
    val pm = new ParameterManager().updated("luby.base", 1)
    
    val luby = new Luby(pm, null)

    Seq.fill(15)(luby.nextRun().value) should contain theSameElementsInOrderAs Seq(1, 1, 2, 1, 1, 2, 4, 1, 1, 2, 1, 1, 2, 4, 8)

  }
}