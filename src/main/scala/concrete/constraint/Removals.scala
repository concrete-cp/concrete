package concrete.constraint;

import java.util.Arrays
import concrete.Domain
import concrete.ProblemState
import concrete.Outcome

trait Removals extends Constraint with AdviseCounts {

  val removals = new Array[Int](arity)

  def advise(problemState: ProblemState, pos: Int): Int = {
    assert(adviseCount >= 0)
    //println(s"advising $this")
    removals(pos) = adviseCount
    getEvaluation(problemState)
  }

  def revise(problemState: ProblemState): Outcome = {
    val r = revise(problemState, modified)
    Arrays.fill(removals, -1)
    r
  }

  def revise(problemState: ProblemState, modified: List[Int]): Outcome

  // scope.iterator.zipWithIndex.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

  def modified: List[Int] = {
    var i = arity - 1
    var mod: List[Int] = Nil
    while (i >= 0) {
      if (removals(i) == adviseCount) {
        mod ::= i
      }
      i -= 1
    }
    assert(mod.nonEmpty)
    mod
  }

  def getEvaluation(problemState: ProblemState): Int

  def skip(modified: List[Int]) = if (modified.tail.isEmpty) modified.head else -1

  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}
