package concrete
package constraint
package semantic

import org.scalatest.{FlatSpec, Inspectors, Matchers}

/**
  * Created by vion on 29/05/17.
  */
class ChannelTest extends FlatSpec with Matchers with Inspectors {

  "Channel" should "be correct" in {
    val x = Array.tabulate(4)(p => new Variable(s"x[${p + 2}]", IntDomain(0 to 20)))

    val prob = new Problem(x)

    val c = new Channel(x, 2)

    prob.addConstraint(c)

    val solv = Solver(prob)

    forAll(solv.toTraversable) { s: Map[Variable, Any] =>
      forAll(2 until 6) { p: Int =>
        val i = s(x(p - 2)).asInstanceOf[Int]
        s(x(i - 2)) shouldBe p
      }
    }


  }

}
