package cspfj.constraint;

import java.util.Arrays
import cspfj.Variable;
import scala.annotation.tailrec

object Removals {
  var count = 0
  def clear() {
    count += 1
  }
}

trait Removals extends Constraint {

  val removals = new Array[Int](arity)

  def advise(pos: Int) = {
    removals(pos) = Removals.count
    getEvaluation
  }

  def revise() = {
    val r = revise(modified(Removals.count, arity - 1))
    Arrays.fill(removals, -1)
    r
  }

  def revise(modified: Seq[Int]): Boolean

  // scope.iterator.zipWithIndex.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

  private def modified(reviseCount: Int, i: Int): Stream[Int] =
    if (i < 0) Stream.empty
    else if (removals(i) == reviseCount) i #:: modified(reviseCount, i - 1)
    else modified(reviseCount, i - 1)

  def getEvaluation: Int

  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}