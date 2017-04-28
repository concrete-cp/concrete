package concrete

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.Tag

import concrete.constraint.linear.Eq
import concrete.constraint.semantic.AllDifferentBC
import concrete.constraint.semantic.Neq
import concrete.heuristic.value.MedValue
import cspom.StatisticsManager
import concrete.heuristic.variable.Dom
import concrete.heuristic.CrossHeuristic
import concrete.heuristic.value.Lexico

object SlowTest extends Tag("concrete.SlowTest")

class TestMAC extends FlatSpec with Matchers {

  def qp(size: Int) = {

    val queens = Array.tabulate(size)(q => new Variable("q" + q, IntDomain(0 until size)))

    //    val qd1 = queens.zipWithIndex map {
    //      case (q, i) => new Variable("d1_" + q.name, IntDomain(-i until size - i))
    //    }
    //
    //    val qd2 = queens.zipWithIndex map {
    //      case (q, i) => new Variable("d2_" + q.name, IntDomain(i until size + i))
    //    }

    val problem = Problem(queens: _*)
    //
    //    for (
    //      ((q, q1, q2), i) <- (queens, qd1, qd2).zipped.toIterable.zipWithIndex
    //    ) {
    //      Eq(false, q, -i, q1).foreach(problem.addConstraint)
    //      Eq(false, q, i, q2).foreach(problem.addConstraint)
    //    }
    //
    //    for (Array(x, y) <- queens.combinations(2)) {
    //      problem.addConstraint(new Neq(x, y))
    //    }

    for (i <- 0 until size; j <- i + 1 until size) {
      problem.addConstraint(new Neq(queens(i), queens(j)))
      problem.addConstraint(new Neq(queens(i), queens(j), i - j))
      problem.addConstraint(new Neq(queens(i), queens(j), j - i))
    }

    (problem, queens)
  }

  //  def allDiff(p: Problem, q: Seq[Variable]) {
  //    // p.addConstraint(new AllDifferentBC(q: _*))
  //
  //    //    p.addConstraint(new AllDifferent2C(q: _*))
  //  }

  val pm = new ParameterManager
  //pm("heuristic.variable") = classOf[Dom]
  pm("heuristic.variable.randomBreak") = false
  val (problem, queens) = qp(88)
  val varh = new Dom(pm, queens.toArray)
  val heuristic = new CrossHeuristic(varh, new Lexico(pm), false)
  val solver = new MAC(problem, pm, heuristic)

  val stats = new StatisticsManager
  stats.register("mac", solver)
  val sol = solver.toStream.headOption
  println(sol)
  println(stats)
  //  def view(queens: Seq[Variable], solution: Map[String, Int]) =
  //    queens.map(q => q.name + " = " + solution.get(q.name)).mkString(", ")
  //
  //  val sols = List(
  //    4 -> 2,
  //    8 -> 92,
  //    9 -> 352,
  //    10 -> 724 //12 -> 14200,
  //    //13 -> 73712 //,
  //    // 14 -> 365596)
  //    )
  //
  //  val pm = new ParameterManager
  //  pm("heuristic.value") = classOf[MedValue]
  //
  //  behavior of "MAC"
  //
  //  for ((size, nb) <- sols) {
  //    it should s"solve queens-$size" taggedAs (SlowTest) in {
  //      val problem = qp(size)
  //
  //      val solver = MAC(problem, pm)
  //
  //      val stats = new StatisticsManager
  //      stats.register("mac", solver)
  //
  //      solver.size shouldBe nb
  //    }
  //  }

}
