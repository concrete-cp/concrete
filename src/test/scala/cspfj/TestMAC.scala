package cspfj
import org.junit.Test
import cspfj.problem.IntDomain
import cspfj.problem.Problem
import cspfj.constraint.semantic.Eq
import cspfj.problem.Variable
import org.junit.Assert
import scala.annotation.tailrec
import cspfj.constraint.semantic.AllDifferentBC
import cspfj.heuristic.MedValue

class TestMAC {

  def qp(size: Int) = {
    val problem = new Problem

    val queens = (0 until size) map (q => problem.addVariable("q" + q, IntDomain(0 until size)))

    problem.addConstraint(allDiff(queens))

    val qd1 = queens.zipWithIndex map {
      case (q, i) =>
        val v = problem.addVariable("d1_" + q.name, IntDomain(-i until size - i))
        problem.addConstraint(new Eq(1, q, -i, v))
        v
    }

    problem.addConstraint(allDiff(qd1))

    val qd2 = queens.zipWithIndex map {
      case (q, i) =>
        val v = problem.addVariable("d2_" + q.name, IntDomain(i until size + i))
        problem.addConstraint(new Eq(1, q, i, v))
        v
    }

    problem.addConstraint(allDiff(qd2))
    (queens, problem)
  }

  def allDiff(q: Seq[Variable]) = new AllDifferentBC(q: _*)

  def view(queens: Seq[Variable], solution: Map[String, Int]) =
    queens.map(q => q.name + " = " + solution.get(q.name)).mkString(", ")

  val sols = Map(
    4 -> 2,
    8 -> 92,
    12 -> 14200);

  @Test //(timeout = 1000)
  def queens() {
    for ((size, nb) <- sols) {
      val (queens, problem) = qp(size)

      ParameterManager("heuristic.value") = classOf[MedValue]
      //ParameterManager("logger.level") = "INFO"
      val solver = new MAC(problem)

      val count = new SolverIterator(solver).size

      Assert.assertEquals(nb, count)
    }
  }
}