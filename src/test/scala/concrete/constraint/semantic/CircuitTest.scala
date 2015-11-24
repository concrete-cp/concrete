package concrete.constraint.semantic

import concrete.IntDomain
import concrete.Variable
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.Problem
import org.scalatest.Inspectors
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink
import org.scalatest.prop.Checkers.check

class CircuitTest extends FlatSpec with Matchers with PropertyChecks {

  "Circuit constraint" should "check" in {
    val v0 = new Variable("v0", IntDomain.ofSeq(0, 1, 2))
    val v1 = new Variable("v1", IntDomain.ofSeq(0, 1, 2))
    val v2 = new Variable("v2", IntDomain.ofSeq(0, 1, 2))

    assert(Circuit(5 -> v0).check(Array(5)))

    val c2 = Circuit(0 -> v0, 1 -> v1)
    assert(c2.check(Array(1, 0)))
    assert(!c2.check(Array(0, 1)))
    assert(!c2.check(Array(0, 0)))
    assert(!c2.check(Array(1, 1)))

    val c3 = Circuit(0 -> v0, 1 -> v1, 2 -> v2)
    for (i <- 0 to 2; j <- 0 to 2; k <- 0 to 2) {
      c3.check(Array(i, j, k)) shouldBe Set((1, 2, 0), (2, 0, 1)).contains((i, j, k))
    }

  }

  it should "work in the presence of absent variables" in {
    val v5 = new Variable("v5", IntDomain.ofSeq(5, 10, 15))
    val v10 = new Variable("v10", IntDomain.ofSeq(5, 10, 15))
    val v15 = new Variable("v15", IntDomain.ofSeq(5, 10, 15))

    val c3 = Circuit(5 -> v5, 10 -> v10, 15 -> v15)

    val p = Problem(v5, v10, v15)
    p.addConstraint(c3)
    val ps = p.initState

    val gen = for (i <- Gen.listOfN(3, Gen.oneOf(5, 10, 15))) yield i

    check {
      forAllNoShrink(gen) {
        case Seq(i, j, k) =>
          c3.check(Array(i, j, k)) == Set((10, 15, 5), (15, 5, 10)).contains((i, j, k))
      }
    }

    val state = ps.assign(v5, 15)
      .assign(v10, 15)
      .assign(v15, 5)
      .toState

    assert(!c3.revise(state).isState)

    check {
      forAllNoShrink(gen) {
        case Seq(i, j, k) =>
          val state = ps.assign(v5, i)
            .assign(v10, j)
            .assign(v15, k)
            .toState

          c3.revise(state).isState == Set((10, 15, 5), (15, 5, 10)).contains((i, j, k))
      }

    }

  }

}