package concrete.constraint.semantic

import concrete.constraint.AdviseCount
import concrete.heuristic.Assign
import concrete._
import org.scalatest.{FlatSpec, Matchers}

class NoGoodsTest extends FlatSpec with Matchers {

  "nogoods" should "find watched" in {
    val v1 = new Variable("v1", IntDomain.ofSeq(1, 2, 3))
    val v2 = new Variable("v2", IntDomain.ofSeq(1, 2, 3))
    val v3 = new Variable("v3", IntDomain.ofSeq(1, 2, 3))
    val problem = Problem(v1, v2, v3)
    val constraint = new NoGoods(Array(v1, v2, v3))
    constraint.register(new AdviseCount())
    val state = problem.initState.toState

    val ng = constraint.addNoGood(Seq(Assign(v1, 1)), v2, IntDomain.ofSeq(2, 3))

    constraint.eventAll(state, InsideRemoval)
//    val mod = constraint.revise(state)
//    mod should be theSameInstanceAs state

    constraint.event(state, Assignment, 1)
    val m2 = constraint.revise(state.assign(v2, 1))

    println(m2)

    assert(!m2.dom(v1).present(1))


  }

}
