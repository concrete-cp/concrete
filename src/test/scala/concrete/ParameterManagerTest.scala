package concrete;

import java.math.BigInteger
import org.junit.Assert.assertEquals
import org.junit.Test
import org.scalatest.FlatSpec
import org.scalatest.Matchers

object ParameterManagerTest {
  @Parameter("classTest")
  val classTest = classOf[Int]

  @Parameter("intTest")
  val intTest = 12
}

final class ParameterManagerTest extends FlatSpec with Matchers {

  val pm = new ParameterManager

  pm.register(ParameterManagerTest)

  "A ParameterManager" should "handle classes" in {
    ParameterManagerTest.classTest shouldBe classOf[Int]
    pm("classTest") = classOf[Double]
    ParameterManagerTest.classTest shouldBe classOf[Double]
    pm("classTest") = "java.math.BigInteger"
    ParameterManagerTest.classTest shouldBe classOf[BigInteger]
  }

  it should "handle ints" in {
    ParameterManagerTest.intTest shouldBe 12
    pm("intTest") = 32
    ParameterManagerTest.intTest shouldBe 32
  }

}
