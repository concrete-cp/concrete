package concrete.constraint;

import java.util.Arrays

import scala.collection.mutable.ArrayBuffer

import concrete.Outcome
import concrete.ProblemState

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
    clearMod()
    r
  }

  def revise(problemState: ProblemState, modified: Seq[Int]): Outcome

  override def isConsistent(problemState: ProblemState): Outcome = {
    isConsistent(problemState, modified)
  }

  def clearMod(): Unit = Arrays.fill(removals, -1)

  def isConsistent(ps: ProblemState, mod: Seq[Int]): Outcome = {
    revise(ps, mod).andThen(_ => ps)
  }

  // scope.iterator.zipWithIndex.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

  def modified: Seq[Int] = {
    var i = 0
    val mod = new ArrayBuffer[Int](arity)
    while (i < arity) {
      if (removals(i) == adviseCount) {
        mod += i
      }
      i += 1
    }
    assert(mod.nonEmpty)
    mod
  }

  def getEvaluation(problemState: ProblemState): Int

  def skip(modified: Seq[Int]) = if (modified.size == 1) modified.head else -1

  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}
