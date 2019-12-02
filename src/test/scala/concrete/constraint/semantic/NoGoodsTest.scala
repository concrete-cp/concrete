package concrete.constraint.semantic

import concrete.constraint.AdviseCount
import concrete.heuristic.Assign
import concrete._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NoGoodsTest extends AnyFlatSpec with Matchers {

  "nogoods" should "find watched" in {
    val v1 = new Variable("v1", IntDomain.ofSeq(1, 2, 3))
    val v2 = new Variable("v2", IntDomain.ofSeq(1, 2, 3))
    val v3 = new Variable("v3", IntDomain.ofSeq(1, 2, 3))
    val problem = Problem(v1, v2, v3)
    val constraint = new NoGoods(Array(v1, v2, v3))
    constraint.register(new AdviseCount())
    val state = problem.initState.toState

    val ng = constraint.addNoGood(Seq(Assign(v1, 1)), v2, IntDomain.ofSeq(2, 3))

    // println(ng)

    constraint.eventAll(state, InsideRemoval)
//    val mod = constraint.revise(state)
//    mod should be theSameInstanceAs state

    constraint.event(state, Assignment, 1)
    val m2 = constraint.revise(state.assign(v2, 1))

    // println(m2)

    assert(!m2.dom(v1).contains(1))


  }

  it should "propagate" in {
    val v82 = new Variable("v82", BooleanDomain.UNKNOWNBoolean)
    val v1 = new Variable("v1", BooleanDomain.UNKNOWNBoolean)
    val problem = Problem(v82, v1)
    val constraint = new NoGoods(Array(v82, v1))
    constraint.register(new AdviseCount())
    val state = problem.initState.toState
    val ng = constraint.addNoGood(Seq(Assign(v82, 0)), v1, BooleanDomain.TRUE)

    constraint.eventAll(state, Assignment)

    val assigned = state.assign(v1, 0) //.assign(v82, 1)
    val m2 = constraint.revise(assigned)

  }

}
