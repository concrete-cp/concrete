package concrete.constraint.linear

;

import concrete.{IntDomain, Problem, Variable}
import concrete.constraint.AdviseCount
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class GtTest extends AnyFlatSpec with Matchers {

  val v1: Variable = new Variable("v1", IntDomain(1 to 4));
  val v2: Variable = new Variable("v2", IntDomain(3 to 5));

  "Gt" should "strictly filter" in {

    val pb = Problem(v1, v2)
    val c = new Gt(v1, v2, true)
    pb.addConstraint(c)
    c.register(new AdviseCount)

    val mod = pb.initState.andThen { ps =>
      c.eventAll(ps)
      c.revise(ps)
    }

    mod.dom(v1).view should contain theSameElementsAs Seq(4)
    mod.dom(v2).view should contain theSameElementsAs Seq(3)
  }

  "Gt" should "not strictly filter" in {
    val pb = Problem(v1, v2)
    val c = new Gt(v1, v2, false)
    pb.addConstraint(c)
    c.register(new AdviseCount)
    val mod = pb.initState.andThen { ps =>
      c.eventAll(ps)
      c.revise(ps)
    }

    mod.dom(v1).view should contain theSameElementsAs Seq(3, 4)
    mod.dom(v2).view should contain theSameElementsAs Seq(3, 4)

  }

}
