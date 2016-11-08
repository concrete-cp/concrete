package concrete.heuristic

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.EntailmentManagerLight
import concrete.IntDomain
import concrete.ParameterManager
import concrete.ProblemState
import concrete.Variable
import concrete.util.Vector

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
    val ps = new ProblemState(Vector(d), Vector(), EntailmentManagerLight(Seq(v)))

    val h = new concrete.heuristic.value.IntervalBranch(new ParameterManager)

    val b = h.branch(v, d, ps)

    b.b1.dom(v).view should contain theSameElementsAs (0 to 1)
    b.b2.dom(v).view should contain theSameElementsAs (10 to 15)

  }

  it should "split single interval" in {
    val d = IntDomain.ofInterval(0, 5)

    val v = new Variable("v", d)
    v.id = 0
    val ps = new ProblemState(Vector(d), Vector(), EntailmentManagerLight(Seq(v)))

    val h = new concrete.heuristic.value.IntervalBranch(new ParameterManager)

    val b = h.branch(v, d, ps)

    b.b1.dom(v).view should contain theSameElementsAs (0 to 2)
    b.b2.dom(v).view should contain theSameElementsAs (3 to 5)

  }

}