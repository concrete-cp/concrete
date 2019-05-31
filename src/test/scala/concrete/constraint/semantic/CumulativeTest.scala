package concrete.constraint.semantic

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.IntDomain
import concrete.Variable
import concrete.Singleton

import scala.util.Random
import concrete.Problem
import concrete.constraint.AdviseCount
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CumulativeTest extends FlatSpec with Matchers with ScalaCheckPropertyChecks {
  "cumulative" should "build profile" in {
    val r = new Random(0)
    val starts = Array.tabulate(5)(s => new Variable(s"s$s", IntDomain(0 until 25)))
    val durations = Array.tabulate(5)(s => new Variable(s"d$s", Singleton(10 + r.nextInt(30))))
    val resources = Array.tabulate(5)(s => new Variable(s"r$s", Singleton(1)))
    val limit = new Variable("l", Singleton(2))

    val problem = new Problem(starts ++ durations ++ resources :+ limit)
    val cumulative = new Cumulative(starts, durations, resources, limit)

    problem.addConstraint(cumulative)
    cumulative.register(new AdviseCount)

    problem.initState.andThen { ps =>
      cumulative.eventAll(ps)
      cumulative.revise(ps)
    }

  }

  it should "filter" in {
    val starts = Array(Singleton(0), Singleton(8), IntDomain(0 to 8)).zipWithIndex.map {
      case (d, i) => new Variable(s"s$i", d)
    }

    val durations = Array(Singleton(1), Singleton(1), Singleton(7)).zipWithIndex.map {
      case (d, i) => new Variable(s"d$i", d)
    }

    val resources = Array.tabulate(3)(s => new Variable(s"r$s", Singleton(1)))
    val limit = new Variable("l", IntDomain(0 to 1))

    val problem = new Problem(starts ++ durations ++ resources :+ limit)
    val cumulative = new Cumulative(starts, durations, resources, limit)

    problem.addConstraint(cumulative)
    cumulative.register(new AdviseCount)

    val mod = problem.initState.andThen { ps =>
      cumulative.eventAll(ps)
      cumulative.revise(ps)
    }

    mod.dom(starts(2)).view should contain theSameElementsAs Seq(1)
    mod.dom(limit).view should contain theSameElementsAs Seq(1)

  }

  it should "filter completely" in {
    val starts = Array(13, 11, 4 to 20, 19, Seq(11, 13, 14, 15, 16, 17, 18, 19, 21), 21)
      .map {
        case i: Int => Singleton(i)
        case r: Seq[Int] => IntDomain.ofSeq(r: _*)
      }
      .zipWithIndex
      .map { case (d, i) => new Variable(s"s$i", d) }

    val dur = Array.tabulate(starts.length)(i => new Variable(s"d$i", Singleton(1)))

    val res = Array.tabulate(starts.length)(i => new Variable(s"r$i", Singleton(1)))

    val bound = new Variable("bound", Singleton(1))

    // -> Cumulative(start = [[11], [13], [12], {2, 3, 4, 5, [12...], 19}, [11], [13], [20], {4, 5, 6, 7, [11...], 20}, [0], [20], [19], [13, 18], [0], [0], [0], [0], [12], [0], [0], [0], [0], [0], [0], [0], [0]], dur = [[t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t]], res = [[f], [t], [f], [f], [t], [f], [f], [t], [f], [f], [t], [t], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f]], bound = true)
    // was revised (
    // -> Cumulative(start = [[11], [13], [12], {2, 3, 4, 5, [12...], 19}, [11], [13], [20], {4, 5, 6, 7, [11...], 20}, [0], [20], [19], [14, 18], [0], [0], [0], [0], [12], [0], [0], [0], [0], [0], [0], [0], [0]], dur = [[t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t]], res = [[f], [t], [f], [f], [t], [f], [f], [t], [f], [f], [t], [t], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f]], bound = true))

    val problem = new Problem(starts ++ dur ++ res :+ bound)
    val cumulative = new Cumulative(starts, dur, res, bound)

    problem.addConstraint(cumulative)
    cumulative.register(new AdviseCount)

    val mod = problem.initState
       .andThen { ps =>
         cumulative.eventAll(ps)
         cumulative.revise(ps) //.andThen(cumulative.revise)
       }

    mod.dom(starts(4)).head shouldBe 14
    mod.dom(starts(4)).last shouldBe 18
  }

  it should "also filter completely" in {
    val starts = Array(135 to 162, 108, 191, 0, 0, 0 to 201, 0, 197 to 199, 0 to 2)
      .map {
        case i: Int => Singleton(i)
        case r: Range => IntDomain(r)
      }
      .zipWithIndex
      .map { case (d, i) => new Variable(s"s$i", d) }

    val dur = Array(153, 83, 124, 123, 135, 114, 106, 116, 197)
      .zipWithIndex
      .map { case (d, i) => new Variable(s"d$i", Singleton(d)) }

    val res = Array(187, 138, 311, 81, 561, 87, 108, 390, 144)
      .zipWithIndex
      .map { case (d, i) => new Variable(s"r$i", Singleton(d)) }

    val bound = new Variable("bound", IntDomain(888 to 991))

    // -> Cumulative(start = [[11], [13], [12], {2, 3, 4, 5, [12...], 19}, [11], [13], [20], {4, 5, 6, 7, [11...], 20}, [0], [20], [19], [13, 18], [0], [0], [0], [0], [12], [0], [0], [0], [0], [0], [0], [0], [0]], dur = [[t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t]], res = [[f], [t], [f], [f], [t], [f], [f], [t], [f], [f], [t], [t], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f]], bound = true)
    // was revised (
    // -> Cumulative(start = [[11], [13], [12], {2, 3, 4, 5, [12...], 19}, [11], [13], [20], {4, 5, 6, 7, [11...], 20}, [0], [20], [19], [14, 18], [0], [0], [0], [0], [12], [0], [0], [0], [0], [0], [0], [0], [0]], dur = [[t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t], [t]], res = [[f], [t], [f], [f], [t], [f], [f], [t], [f], [f], [t], [t], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f], [f]], bound = true))

    val problem = new Problem(starts ++ dur ++ res :+ bound)
    val cumulative = new Cumulative(starts, dur, res, bound)

    problem.addConstraint(cumulative)
    cumulative.register(new AdviseCount)

    val mod = problem.initState.andThen { ps =>
      cumulative.eventAll(ps)
      cumulative.revise(ps)
    }
    mod.dom(bound).head shouldBe 975
  }

}