package concrete.heuristic.restart

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class LubyTest extends FlatSpec with Matchers {
  "Luby" should "generate test sequence" in {
    val luby = new Luby(null, null)

    Seq.fill(15)(luby.nextRun()) should contain theSameElementsInOrderAs Seq(1, 1, 2, 1, 1, 2, 4, 1, 1, 2, 1, 1, 2, 4, 8).map(_ * 100)

  }
}