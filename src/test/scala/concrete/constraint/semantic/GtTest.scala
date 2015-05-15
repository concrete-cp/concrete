package concrete.constraint.semantic;

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.IntDomain
import concrete.Problem
import concrete.Variable

final class GtTest extends FlatSpec with Matchers {

  val v1: Variable = new Variable("v1", IntDomain(1 to 4));
  val v2: Variable = new Variable("v2", IntDomain(3 to 5));

  "Gt" should "strictly filter" in {

    val pb = Problem(v1, v2)
    val c = new Gt(v1, v2, true);
    pb.addConstraint(c)
    val ps = pb.initState.toState

    val mod = c.consistentRevise(ps);

    mod.dom(v1) shouldBe Seq(4)
    mod.dom(v2) shouldBe Seq(3)
  }

  "Gt" should "not strictly filter" in {
    val pb = Problem(v1, v2)
    val c = new Gt(v1, v2, false);
    pb.addConstraint(c)
    val ps = pb.initState.toState

    val mod = c.consistentRevise(ps);

    mod.dom(0) shouldBe Seq(3, 4)
    mod.dom(1) shouldBe Seq(3, 4)

  }

}
