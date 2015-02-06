package concrete

import concrete.constraint.semantic.LexLeq
import concrete.constraint.semantic.Gt
import concrete.constraint.Residues
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator

object Matching extends App {

  val n = 2

  val couples = for (i <- 0 until n) yield (
    new Variable(s"$i.0", IntDomain(0 until 2 * n)),
    new Variable(s"$i.1", IntDomain(0 until 2 * n)))

  val problem = Problem(couples.flatMap { case (i, j) => Seq(i, j) }: _*)

  for ((v1, v2) <- couples) {
    problem.addConstraint(new Gt(v2, v1, true))
  }

  for (Seq(c1, c2) <- couples.sliding(2)) {
    problem.addConstraint(new Gt(c2._1, c1._1, true))
  }

  val solver = Solver(problem)

  for (s <- solver) {
    println(s)
  }

}