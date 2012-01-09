package cspfj
import org.junit.Test
import cspfj.problem.BitVectorDomain
import cspfj.problem.Problem
import cspfj.constraint.semantic.Eq
import cspfj.constraint.semantic.AllDifferent
import cspfj.problem.Variable
import org.junit.Assert
import scala.annotation.tailrec

class TestMAC {

  def qp(size: Int) = {
    val problem = new Problem

    val queens = (0 until size) map (q => problem.addVariable("q" + q, new BitVectorDomain(0 until size: _*)))

    problem.addConstraint(new AllDifferent(queens: _*))

    val qd1 = queens.zipWithIndex map {
      case (q, i) =>
        val v = problem.addVariable("d1_" + q.name, new BitVectorDomain(0 - i until size - i: _*))
        problem.addConstraint(new Eq(1, q, -i, v))
        v
    }

    problem.addConstraint(new AllDifferent(qd1: _*))

    val qd2 = queens.zipWithIndex map {
      case (q, i) =>
        val v = problem.addVariable("d2_" + q.name, new BitVectorDomain(0 + i until size + i: _*))
        problem.addConstraint(new Eq(1, q, i, v))
        v
    }

    problem.addConstraint(new AllDifferent(qd2: _*))
    (queens, problem)
  }

  //  @Test //(timeout = 1000)
  //  def queens() {
  //    val (queens, problem) = qp(4)
  //
  //    val solver = new MAC(problem)
  //    solver.maxBacktracks = -1
  //    solver.prepare()
  //
  //    @tailrec
  //    def count(modified: Variable, stack: List[Pair], c: Int): Int = {
  //      solver.mac(modified, stack) match {
  //        case (None, _) => c
  //        case (Some(solution), stack) => {
  //          if (stack == Nil) c + 1
  //          else {
  //            stack.head.remove()
  //            count(stack.head.variable, stack, c + 1)
  //          }
  //        }
  //      }
  //
  //    }
  //
  //    Assert.assertEquals(14200, count(null, Nil, 0))
  //  }

  def view(queens: Seq[Variable], solution: Map[String, Int]) =
    queens.map(q => q.name + " = " + solution.get(q.name)).mkString(", ")

  val sols = Map(
    4 -> 2,
    8 -> 92,
    12 -> 14200,
    13 -> 73712);

  @Test //(timeout = 1000)
  def queens2() {
    val size = 12
    val (queens, problem) = qp(size)
    //ParameterManager.parameter("logger.level", "FINE")
    val solver = new MAC(problem)

    @tailrec
    def count(c: Int): Int = {

      solver.nextSolution() match {
        case None => c
        case Some(solution) => {
          //println(c + " : " + view(queens, solution))
          count(c + 1)
        }
      }

    }

    Assert.assertEquals(sols(size), count(0))
  }
}