package concrete.constraint.semantic

import concrete.{Assignment, IntDomain, Problem, Variable}
import concrete.constraint.AdviseCount
import cspom.variable.IntVariable
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.Checkers.check
import org.scalatest.prop.PropertyChecks

class CircuitTest extends FlatSpec with Matchers with PropertyChecks {

  "Circuit constraint" should "check" in {
    val length = 10
    val vars = Array.tabulate(length)(i =>
      new Variable(s"v$i", IntDomain.ofInterval(0, length - 1))
    )

    val circuit = new Circuit(vars, 0, new Variable("size", IntDomain.ofInterval(2, 10)))

    assert(circuit.check(Array(1, 2, 3, 4, 0, 5, 6, 7, 8, 9, 5)))
    assert(circuit.check(Array(1, 2, 5, 3, 4, 0, 6, 7, 8, 9, 4)))
    //    assert(Circuit(5 -> v0).check(Array(5)))
    //
    //    val c2 = Circuit(0 -> v0, 1 -> v1)
    //    assert(c2.check(Array(1, 0)))
    //    assert(!c2.check(Array(0, 1)))
    //    assert(!c2.check(Array(0, 0)))
    //    assert(!c2.check(Array(1, 1)))
    //
    //    val c3 = Circuit(0 -> v0, 1 -> v1, 2 -> v2)
    //    for (i <- 0 to 2; j <- 0 to 2; k <- 0 to 2) {
    //      c3.check(Array(i, j, k)) shouldBe Set((1, 2, 0), (2, 0, 1)).contains((i, j, k))
    //    }

  }

  it should "accept case" in {
    val domains = Array(
      IntDomain.ofSeq(0, 1, 3),
      IntDomain.ofSeq(0, 1, 2, 4),
      IntDomain.ofSeq(1, 2, 5),
      IntDomain.ofSeq(0, 3, 4, 6),
      IntDomain.ofSeq(1, 3, 4, 5, 7),
      IntDomain.ofSeq(2, 4, 5, 8),
      IntDomain.ofSeq(3, 6, 7),
      IntDomain.ofSeq(4, 6, 7, 8),
      IntDomain.ofSeq(5, 7)
    )
    val size = new Variable("size", IntDomain.ofInterval(2, 9))
    val vars = for ((d, i) <- domains.zipWithIndex) yield new Variable(s"v$i", d)

    val problem = Problem(vars :+ size: _*)

    val circuit = new Circuit(vars, 0, size)
    problem.addConstraint(circuit)
    circuit.register(new AdviseCount)
    val ps = problem.initState.toState

    circuit.eventAll(ps, Assignment, alwaysMod = true)

    withClue(circuit.toString(ps)) {
      val mod = circuit.revise(ps)
      assert(mod.isState)
      // println(circuit.toString(mod.toState))
    }

  }


  //  it should "work in the presence of absent variables" in {
  //    val v5 = new Variable("v5", IntDomain.ofSeq(5, 10, 15))
  //    val v10 = new Variable("v10", IntDomain.ofSeq(5, 10, 15))
  //    val v15 = new Variable("v15", IntDomain.ofSeq(5, 10, 15))
  //
  //    val c3 = Circuit(5 -> v5, 10 -> v10, 15 -> v15)
  //
  //    val p = Problem(v5, v10, v15)
  //    p.addConstraint(c3)
  //    c3.register(new AdviseCount)
  //    val ps = p.initState
  //
  //    val gen = for (i <- Gen.listOfN(3, Gen.oneOf(5, 10, 15))) yield i
  //
  //    check {
  //      forAllNoShrink(gen) {
  //        case Seq(i, j, k) =>
  //          c3.check(Array(i, j, k)) == Set((10, 15, 5), (15, 5, 10)).contains((i, j, k))
  //      }
  //    }
  //
  //    val state = ps.assign(v5, 15)
  //      .assign(v10, 15)
  //      .assign(v15, 5)
  //      .andThen { state =>
  //        c3.eventAll(state)
  //        c3.revise(state)
  //      }
  //    assert(!state.isState)
  //
  //    check {
  //      forAllNoShrink(gen) {
  //        case Seq(i, j, k) =>
  //          val state = ps.assign(v5, i)
  //            .assign(v10, j)
  //            .assign(v15, k)
  //            .toState
  //          c3.eventAll(state)
  //          c3.revise(state).isState == Set((10, 15, 5), (15, 5, 10)).contains((i, j, k))
  //      }
  //
  //    }
  //
  //  }

}