package cspfj;

import java.math.BigInteger

import org.junit.Assert.assertEquals
import org.junit.Test

object ParameterManagerTest {
  @Parameter("classTest")
  val classTest = classOf[Int]

  @Parameter("intTest")
  val intTest = 12

  ParameterManager.register(this)
}

final class ParameterManagerTest {

  @Test
  def classTest() {
    assertEquals(classOf[Int], ParameterManagerTest.classTest);
    ParameterManager("classTest") = classOf[Double]
    assertEquals(classOf[Double], ParameterManagerTest.classTest);
    ParameterManager.parse("classTest", "java.math.BigInteger");
    assertEquals(classOf[BigInteger], ParameterManagerTest.classTest);
  }

  @Test
  def intTest() {
    assertEquals(12, ParameterManagerTest.intTest);
    ParameterManager("intTest") = 32
    assertEquals(32, ParameterManagerTest.intTest);
  }

}
