package concrete

import scala.annotation.tailrec
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.Tag
import concrete.constraint.semantic.AllDifferent2C
import concrete.constraint.semantic.AllDifferentBC
import concrete.constraint.linear.EqAC
import concrete.constraint.linear.EqBC
import concrete.heuristic.MedValue
import cspom.StatisticsManager
import concrete.heuristic.Split

object SlowTest extends Tag("concrete.SlowTest")

class TestMAC extends FlatSpec with Matchers {

  def qp(size: Int) = {

    val queens = (0 until size)
      .map(q => new Variable("q" + q, IntDomain(0 until size)))
      .toList

    val qd1 = queens.zipWithIndex map {
      case (q, i) => new Variable("d1_" + q.name, IntDomain(-i until size - i))
    }

    val qd2 = queens.zipWithIndex map {
      case (q, i) => new Variable("d2_" + q.name, IntDomain(i until size + i))
    }

    val problem = new Problem(queens ::: qd1 ::: qd2)

    for (((q, q1, q2), i) <- (queens, qd1, qd2).zipped.toIterable.zipWithIndex) {
      problem.addConstraint(new EqAC(false, q, -i, q1))
      problem.addConstraint(new EqAC(false, q, i, q2))
      problem.addConstraint(new EqBC(false, q, -i, q1))
      problem.addConstraint(new EqBC(false, q, i, q2))
    }

    allDiff(problem, queens)
    allDiff(problem, qd1)
    allDiff(problem, qd2)
    problem
  }

  def allDiff(p: Problem, q: Seq[Variable]) {
    p.addConstraint(new AllDifferentBC(q: _*))
    p.addConstraint(new AllDifferent2C(q: _*))
  }

  def view(queens: Seq[Variable], solution: Map[String, Int]) =
    queens.map(q => q.name + " = " + solution.get(q.name)).mkString(", ")

  val sols = List(
    4 -> 2,
    8 -> 92,
    12 -> 14200)

  val pm = new ParameterManager
  pm("heuristic.value") = classOf[MedValue]

  behavior of "MAC"

  for ((size, nb) <- sols) {
    it should s"solve queens-$size" taggedAs (SlowTest) in {
      val problem = qp(size)

      val solver = MAC(problem, pm)

      val stats = new StatisticsManager
      stats.register("mac", solver)

      solver.size shouldBe nb
    }
  }

}
