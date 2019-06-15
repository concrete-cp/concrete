package concrete.constraint.semantic

import concrete.constraint.AdviseCount
import concrete.{IntDomain, Problem, Variable}
import org.scalatest.FlatSpec

class FZSubcircuitTest extends FlatSpec {

  behavior of "FZSubcircuitTest"

  it should "check" in {
    val constraint = new FZSubcircuit(Array.fill(5)(null))

    // One loop
    assert(constraint.check(Array(2, 3, 4, 5, 1)))
    assert(constraint.check(Array(5, 1, 2, 3, 4)))

    // No loop
    assert(constraint.check(Array(1, 2, 3, 4, 5)))

    // One subloop
    assert(constraint.check(Array(1, 2, 4, 5, 3)))

    // Two subloops
    assert(!constraint.check(Array(2, 1, 4, 5, 3)))
    assert(!constraint.check(Array(2, 1, 4, 3, 5)))

  }

  it should "revise" in {
//    val variables = Array(
//      new Variable(s"x1", IntDomain.ofSeq(1, 2, 3)),
//      new Variable(s"x2", IntDomain.ofSeq(2, 3, 4, 5)),
//      new Variable(s"x3", IntDomain.ofSeq(3, 4)),
//      new Variable(s"x4", IntDomain.ofSeq(3, 5)),
//      new Variable(s"x5", IntDomain.ofSeq(4)),
//    )

    val variables = Array(
      new Variable(s"x1", IntDomain.ofSeq(2)),
      new Variable(s"x2", IntDomain.ofSeq(2)),
      new Variable(s"x3", IntDomain.ofSeq(3)),
      new Variable(s"x4", IntDomain.ofSeq(4)),
      new Variable(s"x5", IntDomain.ofSeq(5)),
    )

    val constraint = new FZSubcircuit(variables)
    constraint.register(new AdviseCount)

    val prob = new Problem(variables)
    prob.addConstraint(constraint)

    val mod = prob.initState
      .andThen { ps =>
        val scc = constraint.buildGraph(ps).findAllSCC()
        println(scc.toSeq)

        constraint.eventAll(ps)
        constraint.revise(ps)
      }
      .toState

    println(mod)
    constraint.eventAll(mod)
    assert(constraint.revise(mod) == mod)

  }

}
