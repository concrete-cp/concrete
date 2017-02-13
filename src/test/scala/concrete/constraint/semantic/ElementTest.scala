package concrete.constraint.semantic

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.IntDomain
import concrete.Variable
import concrete.Problem
import concrete.InsideRemoval
import concrete.constraint.AdviseCount

/**
 * @author vion
 */
class ElementTest extends FlatSpec with Matchers {

  "Element constraint" should "not create new domains when NOP" in {
    val r = new Variable("r", IntDomain.ofInterval(6, 14))
    val i = new Variable("i", IntDomain.ofSeq(2, 4, 15))
    val a2 = new Variable("a2", IntDomain.ofSeq(3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
  }

  it should "filter correctly test case" in {
    val r = new Variable("r", IntDomain.ofSeq(47, 59, 65))
    val i = new Variable("i", IntDomain.ofSeq(10, 13))
    val v10 = new Variable("v10", IntDomain.ofSeq(65))
    val v13 = new Variable("v13", IntDomain.ofSeq(59, 65))
    val c = new ElementWatch(r, i, Array(null, null, null, null, null, null, null, null, null, null, v10, null, null, v13, v13))

    c.resultWatches ++= Map(59 -> 13, 65 -> 10, 47 -> 13)
    c.watched(10) = 1
    c.watched(13) = 2

    val problem = Problem(r, i, v10, v13)
    problem.addConstraint(c)
    c.register(new AdviseCount)

    val state = problem.initState.toState

    c.adviseArray(state, InsideRemoval, Array(3, 4))
    val mod = c.revise(state)
    mod.dom(r).view should contain theSameElementsAs Seq(59, 65)
    //th of ArrayBuffer({}, [47], [59], [65], [65], [65], [65], [65], [65], [65], [65], [65], [65], {59, 65}, {59, 65}, {47, 59, 65}) -> NOP

  }

}