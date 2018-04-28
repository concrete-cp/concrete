package concrete.constraint.semantic

import com.typesafe.scalalogging.LazyLogging
import concrete._
import concrete.constraint.AdviseCount
import cspom._
import cspom.variable.IntVariable
import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.util.Random

class DiffNTest extends FlatSpec with Matchers with OptionValues with LazyLogging {
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

  it should "run" in {

    val r = new Random(0)

    import CSPOM._
    import CSPOMDriver._

    val n = 20
    val sizes = IndexedSeq.fill(n)(
      Seq(1 + r.nextInt(5), 1 + r.nextInt(5)))

    val cspom = CSPOM { implicit problem =>

      val max = Seq(IntVariable(0 until 100) as "maxX",
       IntVariable(0 until 100) as "maxY")

      ctr(max(0) === max(1))
      // val obj = (maxX * maxY) as "obj"

      val coordinates = Seq.tabulate(n)(i =>
        Seq(IntVariable(0 until 100) as s"x$i",
          IntVariable(0 until 100) as s"y$i"))

      for (i <- 0 until n; dim <- 0 until 2) {
        ctr(coordinates(i)(dim) + sizes(i)(dim) <= max(dim))
      }

      ctr(CSPOMConstraint('diffn)(coordinates.map(seq2CSPOMSeq(_)), sizes.map(seq2CSPOMSeq(_))))

      goal(CSPOMGoal.Minimize(max(0)))
    }
    val pm = new ParameterManager().updated("heuristic.value", Seq())
    val solver = Solver(cspom, pm).get

    val stats = new StatisticsManager
    stats.register("solver", solver.solver)

    //      for (sol <- solver.toIterable) {
    //        for (i <- 0 until n) println((sol.get(s"x$i").get, sol.get(s"y$i").get, dxs(i), dys(i)))
    //        println(sol.get("maxX"))
    //        println("------")
    //      }
    solver.toTraversable.lastOption.value("maxX") shouldBe 14

    logger.info(stats.toString)


  }
}