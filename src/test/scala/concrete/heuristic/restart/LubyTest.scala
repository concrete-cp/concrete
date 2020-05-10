package concrete.heuristic.restart

import org.scalatest.OptionValues
import concrete.ParameterManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LubyTest extends AnyFlatSpec with Matchers with OptionValues {
  "Luby" should "generate test sequence" in {
    val pm = new ParameterManager().updated("luby.base", 1)
    
    val luby = new Luby(pm, null)

    Seq.fill(15)(luby.nextRun().value) should contain theSameElementsInOrderAs Seq(1, 1, 2, 1, 1, 2, 4, 1, 1, 2, 1, 1, 2, 4, 8)

  }
}