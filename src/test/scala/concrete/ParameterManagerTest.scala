package concrete

;

import java.math.BigInteger

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParameterManagerTester(private val pm: ParameterManager) {
  val classTest: Class[_] = pm.getOrElse("classTest", classOf[Int])

  val intTest: Int = pm.getOrElse("intTest", 12)
}

final class ParameterManagerTest extends AnyFlatSpec with Matchers {


  "A ParameterManager" should "handle classes" in {
    var pm = new ParameterManager
    new ParameterManagerTester(pm).classTest shouldBe classOf[Int]
    pm = pm.updated("classTest", classOf[Double])
    new ParameterManagerTester(pm).classTest shouldBe classOf[Double]
    pm = pm.updated("classTest", "java.math.BigInteger")
    new ParameterManagerTester(pm).classTest shouldBe classOf[BigInteger]
  }

  it should "handle ints" in {
    var pm = new ParameterManager
    new ParameterManagerTester(pm).intTest shouldBe 12
    pm = pm.updated("intTest", 32)
    new ParameterManagerTester(pm).intTest shouldBe 32
    pm = pm.updated("intTest", "64")
    new ParameterManagerTester(pm).intTest shouldBe 64
  }

}
