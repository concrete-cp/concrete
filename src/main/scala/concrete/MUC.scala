package concrete

import concrete.constraint.Constraint

object MUC {
  def transition(ps: ProblemState, constraints: Seq[Constraint], solver: MAC, min: Int, max: Int, act: IndexedSeq[Int]): Int = {
    if (min < max) {
      val middle = (min + max) / 2
      println(act.take(middle))
      val entailed = applyActive(ps, constraints, act.take(middle).toSet)

      solver.reset()
      if (solver.hasNext) {
        transition(ps, constraints, solver, middle + 1, max, act)
      } else {
        transition(ps, constraints, solver, min, middle, act)
      }
    } else {
      //      require(filter.reduceAll(applyActive(ps, constraints, act.take(min - 1).toSet)).isState)
      //      require(!filter.reduceAll(applyActive(ps, constraints, act.take(min).toSet)).isState)

      min - 1
    }
  }

  def applyActive(ps: ProblemState, constraints: Seq[Constraint], act: Set[Int]): ProblemState = {
    constraints.filter(c => !act(c.id)).foldLeft(ps)(_.entail(_))
  }

  def apply(pb: Problem, solver: MAC, state: ProblemState) = {

    var act: IndexedSeq[Int] = 0 until pb.constraints.size

    var k = 0
    while (k < act.size) {

      val c = transition(state, pb.constraints, solver, k, act.size, act)
      //println(c)
      act = act(c) +: act.take(c)
      k += 1
      //println(act)
      //println((k, act.size))
    }
    act.map(pb.constraints)
  }
}