package cspfj;

import org.junit.Assert.assertEquals
import org.junit.Test

object StatisticsManagerTest {
  @Statistic
  val testInt = 8;
  @Statistic
  val testLong = 9l;
  @Statistic
  val testFloat = 10.0f;
  @Statistic
  val testDouble = 11.0;
}

final class StatisticsManagerTest {

  @Test
  def testRegisterStringObject() {
    StatisticsManager.register("test", StatisticsManagerTest);
    assertEquals(8, StatisticsManager.get("test.testInt"));
    assertEquals(9l, StatisticsManager.get("test.testLong"));
    assertEquals(10f, StatisticsManager.get("test.testFloat"));
    assertEquals(11d, StatisticsManager.get("test.testDouble"));
  }

  @Test
  def testOps() {
    val seq = Seq(30, 53, 76, 77, 25, 66, 78, 50,
      70, 42, 90, 31, 79, 7, 39, 49, 15, 82, 35, 27)

    assertEquals(51.05, StatisticsManager.average(seq), 0)
    assertEquals(50, StatisticsManager.median(seq), 0)
    assertEquals(24.81187, StatisticsManager.stDev(seq), 0.00001)
  }

}
