package concrete
import org.junit.Test
import concrete.constraint.semantic.Eq
import org.junit.Assert
import scala.annotation.tailrec
import concrete.constraint.semantic.AllDifferentBC
import concrete.heuristic.MedValue
import concrete.constraint.semantic.AllDifferent2C

class TestMAC {

  def qp(size: Int) = {

    val queens = (0 until size) map (q => new Variable("q" + q, IntDomain(0 until size))) toList

    val qd1 = queens.zipWithIndex map {
      case (q, i) => new Variable("d1_" + q.name, IntDomain(-i until size - i))
    }

    val qd2 = queens.zipWithIndex map {
      case (q, i) => new Variable("d2_" + q.name, IntDomain(i until size + i))
    }

    val problem = new Problem(queens ::: qd1 ::: qd2)

    for (((q, q1, q2), i) <- (queens, qd1, qd2).zipped.toIterable.zipWithIndex) {
      problem.addConstraint(new Eq(false, q, -i, q1))
      problem.addConstraint(new Eq(false, q, i, q2))
    }

    allDiff(problem, queens)
    allDiff(problem, qd1)
    allDiff(problem, qd2)
    (queens, problem)
  }

  def allDiff(p: Problem, q: Seq[Variable]) {
    p.addConstraint(new AllDifferentBC(q: _*))
    //p.addConstraint(new AllDifferent2C(q: _*))
  }

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

      val count = solver.size

      Assert.assertEquals(nb, count)
    }
  }
}
