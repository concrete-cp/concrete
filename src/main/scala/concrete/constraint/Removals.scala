package concrete.constraint;

import java.util.Arrays
import concrete.ReviseOutcome
import concrete.Domain

trait Removals extends Constraint with AdviseCounts {

  val removals = new Array[Int](arity)

  def advise(domains: IndexedSeq[Domain], pos: Int) = {
    removals(pos) = adviseCount
    getEvaluation(domains)
  }

  def revise(domains: IndexedSeq[Domain], state: State): ReviseOutcome[State] = {
    val r = revise(domains, modified, state)
    Arrays.fill(removals, -1)
    r
  }

  def revise(domains: IndexedSeq[Domain], modified: List[Int], state: State): ReviseOutcome[State]

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
    mod
  }

  def getEvaluation(domains: IndexedSeq[Domain]): Int

  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}
