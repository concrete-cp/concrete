package concrete.constraint.semantic

import concrete.{IntDomain, Problem, Singleton, Variable}
import concrete.constraint.AdviseCount
import org.scalatest.{FlatSpec, Matchers}

class DiffNTest extends FlatSpec with Matchers {
  "diffN" should "filter" in {
    val xs = Array(Singleton(1), Singleton(2), IntDomain(0 until 15)).zipWithIndex.map {
      case (d, i) => new Variable(s"x$i", d)
    }

    val ys = Array(Singleton(0), Singleton(1), IntDomain(0 until 2)).zipWithIndex.map {
      case (d, i) => new Variable(s"y$i", d)
    }

    val dxs = Array(Singleton(1), Singleton(1), Singleton(3)).zipWithIndex.map {
      case (d, i) => new Variable(s"dx$i", d)
    }

    val dys = Array(Singleton(1), Singleton(2), Singleton(2)).zipWithIndex.map {
      case (d, i) => new Variable(s"dy$i", d)
    }

    val problem = new Problem(xs ++ ys ++ dxs ++ dys)
    val diffn = new DiffN(xs.reverse, ys.reverse, dxs.reverse, dys.reverse)

    problem.addConstraint(diffn)
    diffn.register(new AdviseCount)


    val mod = problem.initState
      .andThen { ps =>
        diffn.eventAll(ps)
        diffn.revise(ps)
      }
      .toState

    mod.dom(xs(2)).head shouldBe 3

  }
}