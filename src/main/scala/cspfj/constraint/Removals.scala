package cspfj.constraint;

import java.util.Arrays
import cspfj.problem.Variable;
import scala.annotation.tailrec

object Removals {
  var count = 0
  def clear() {
    count += 1
  }
}

trait Removals extends Constraint {

  val removals = new Array[Int](arity)

  fillRemovals()

  final override def setRemovals(p: Int) {
    removals(p) = Removals.count
  }

  final override def clearRemovals() {
    Arrays.fill(removals, -1)
  }

  final override def revise() {
    revise(modified(Removals.count, arity - 1))
  }

  override def fillRemovals() {
    Arrays.fill(removals, Removals.count)
  }

  def revise(modified: Seq[Int])

  // scope.iterator.zipWithIndex.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

  private def modified(reviseCount: Int, i: Int): Stream[Int] =
    if (i < 0) Stream.empty
    else if (removals(i) == reviseCount) i #:: modified(reviseCount, i - 1)
    else modified(reviseCount, i - 1)

  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}