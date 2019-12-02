package concrete

import concrete.constraint.linear.Eq
import concrete.constraint.semantic.AllDifferentAC
import concrete.heuristic.value.MedValue
import cspom.StatisticsManager
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestMAC extends AnyFlatSpec with Matchers {

  val sols: List[(Int, Int, Option[Tag])] = List(
    (4, 2, None),
    (8, 92, None),
    (9, 352, Some(SlowTest)),
    (10, 724, Some(SlowTest)),
    (12, 14200, Some(SlowTest)),
    //13 -> 73712 //,
    // 14 -> 365596)
  )
  val qs = 200
  private val pm = new ParameterManager().updated("heuristic.value", classOf[MedValue])

  def qp(size: Int): Problem = {

    val queens = (0 until size)
      .map(q => new Variable("q" + q, IntDomain(0 until size)))

    val qd1 = queens.zipWithIndex map {
      case (q, i) => new Variable("d1_" + q.name, IntDomain(-i until size - i))
    }

    val qd2 = queens.zipWithIndex map {
      case (q, i) => new Variable("d2_" + q.name, IntDomain(i until size + i))
    }

    val problem = Problem(queens ++ qd1 ++ qd2: _*)

    for (
      ((q, q1, q2), i) <- (queens, qd1, qd2).zipped.toIterable.zipWithIndex
    ) {
      Eq(neg = false, q, -i, q1).foreach(problem.addConstraint)
      Eq(neg = false, q, i, q2).foreach(problem.addConstraint)
    }

    allDiff(problem, queens)
    allDiff(problem, qd1)
    allDiff(problem, qd2)
    problem
  }

  def allDiff(p: Problem, q: Seq[Variable]): Unit = {
    //    p.addConstraint(new AllDifferentBC(q.toArray))
    //    for (Seq(x, y) <- q.combinations(2)) {
    //      p.addConstraint(new Neq(x, y))
    //    }
    p.addConstraint(new AllDifferentAC(q: _*))
  }

  behavior of "MAC"

  for ((size, nb, tag) <- sols) {
    it should s"solve queens-$size" taggedAs tag.getOrElse(Tag("normal")) in {
      val problem = qp(size)

      val solver = MAC(problem, problem.variables, pm).get

      val stats = new StatisticsManager
      stats.register("mac", solver)

      solver.size shouldBe nb
    }
  }

  def view(queens: Seq[Variable], solution: Map[String, Int]): String =
    queens.map(q => q.name + " = " + solution.get(q.name)).mkString(", ")

  it should s"solve queens-$qs quickly" taggedAs SlowTest in {
    val problem = qp(qs)

    val solver = MAC(problem, problem.variables, pm).get

    val stats = new StatisticsManager
    stats.register("mac", solver)


    assert(solver.nonEmpty)
    // println(stats)

  }

}
