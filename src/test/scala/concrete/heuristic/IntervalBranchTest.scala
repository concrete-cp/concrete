package concrete.heuristic

import concrete.{EntailmentManager, IntDomain, ProblemState, Variable}
import concrete.heuristic.branch.IntervalBranch
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.IntMap

/**
  * @author vion
  */
class IntervalBranchTest extends FlatSpec with Matchers {
  "Interval heuristic" should "select two intervals" in {
    val i = IntDomain.ofInterval(0, 1)
    val j = IntDomain.ofInterval(10, 15)
    val d = i | j

    val v = new Variable("v", d)
    v.id = 0
    val ps = new ProblemState(
      domains = IntMap(0 -> d),
      entailed = EntailmentManager(Seq(v)))

    val h = new IntervalBranch()

    val Right((_, b1, b2)) = h.branch(v, d, ps)

    b1(ps)._1.dom(v).view should contain theSameElementsAs (0 to 1)
    b2(ps)._1.dom(v).view should contain theSameElementsAs (10 to 15)

  }

  it should "split single interval" in {
    val d = IntDomain.ofInterval(0, 5)

    val v = new Variable("v", d)
    v.id = 0
    val ps = new ProblemState(
      domains = IntMap(0 -> d),
      entailed = EntailmentManager(Seq(v)))

    val h = new IntervalBranch()

    val Right((_, b1, b2)) = h.branch(v, d, ps)

    b1(ps)._1.dom(v).view should contain theSameElementsAs (0 to 2)
    b2(ps)._1.dom(v).view should contain theSameElementsAs (3 to 5)

  }

}