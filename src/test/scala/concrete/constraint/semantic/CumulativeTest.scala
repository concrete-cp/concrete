package concrete.constraint.semantic

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import concrete.IntDomain
import concrete.Variable
import concrete.Singleton
import scala.util.Random
import concrete.Problem

class CumulativeTest extends FlatSpec with Matchers {
  "cumulative" should "build profile" in {
    val r = new Random(0)
    val starts = Array.tabulate(5)(s => new Variable(s"s$s", IntDomain(0 until 25)))
    val durations = Array.tabulate(5)(s => new Variable(s"d$s", Singleton(10 + r.nextInt(30))))
    val resources = Array.tabulate(5)(s => new Variable(s"r$s", Singleton(1)))
    val limit = new Variable("l", Singleton(2))

    val problem = new Problem(starts ++ durations ++ resources :+ limit)
    val cumulative = new Cumulative(starts, durations, resources, limit)

    problem.addConstraint(cumulative)

    val ps = problem.initState.toState

    val mod = cumulative.revise(ps)

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

    val ps = problem.initState.toState

    val mod = cumulative.revise(ps)

    mod.dom(starts(2)).view should contain theSameElementsAs Seq(1)
    mod.dom(limit).view should contain theSameElementsAs Seq(1)

  }
}