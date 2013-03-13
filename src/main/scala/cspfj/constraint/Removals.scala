package cspfj.constraint;

import java.util.Arrays
import cspfj.AdviseCount
import cspfj.Variable

trait Removals extends Constraint {

  val removals = new Array[Int](arity)

  def advise(pos: Int) = {
    removals(pos) = AdviseCount.count
    getEvaluation
  }

  def revise() = {
    val r = revise(modified(AdviseCount.count, arity - 1))
    Arrays.fill(removals, -1)
    r
  }

  def revise(modified: List[Int]): Traversable[Int]

  // scope.iterator.zipWithIndex.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

  private def modified(reviseCount: Int, i: Int): List[Int] =
    if (i < 0) {
      Nil
    } else if (removals(i) == reviseCount) {
      i :: modified(reviseCount, i - 1)
    } else {
      modified(reviseCount, i - 1)
    }

  def getEvaluation: Int

  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}
