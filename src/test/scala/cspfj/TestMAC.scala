package cspfj
import org.junit.Test
import cspfj.problem.BitVectorDomain
import cspfj.problem.Problem
import cspfj.constraint.semantic.Eq
import cspfj.constraint.semantic.AllDifferent
import cspfj.problem.Variable
import org.junit.Assert

class TestMAC {
  @Test
  def queens() {
    val problem = new Problem

    val size = 3

    val queens = (0 to size) map (q => problem.addVariable("q" + q, new BitVectorDomain(0 to size: _*)))

    problem.addConstraint(new AllDifferent(queens: _*))

    val qd1 = queens.zipWithIndex map {
      case (q, i) =>
        val v = problem.addVariable("d1_" + q.name, new BitVectorDomain(0 - i to size - i: _*))
        problem.addConstraint(new Eq(1, q, -i, v))
        v
    }

    problem.addConstraint(new AllDifferent(qd1: _*))

    val qd2 = queens.zipWithIndex map {
      case (q, i) =>
        val v = problem.addVariable("d2_" + q.name, new BitVectorDomain(0 + i to size + i: _*))
        problem.addConstraint(new Eq(1, q, i, v))
        v
    }

    problem.addConstraint(new AllDifferent(qd2: _*))

    val solver = new MAC(problem)
    solver.maxBacktracks = -1
    solver.prepare()

    def count(stack: List[Pair], c: Int): Int = {
      val solution = if (stack == Nil) {
        solver.mac(null, Nil)
      } else {
        problem.pop()
        stack.head.variable.dom.remove(stack.head.index)
        solver.mac(stack.head.variable, stack.tail)
      }
      if (solution._1 == None) {
        c
      } else {
        print(c + " : ")
        view(queens, solution)
        count(solution._2, c + 1)
      }
    }

    Assert.assertEquals(92, count(Nil, 0))
  }

  def view(queens: Seq[Variable], solution: (Option[Map[String, Int]], List[Pair])) {
    //println(solution._2)
    println(queens.map(q => q.name + " = " + solution._1.get(q.name)).mkString(", "))
  }
}