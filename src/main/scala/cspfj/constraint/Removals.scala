package cspfj.constraint;

import java.util.Arrays
import cspfj.problem.Variable;
import scala.annotation.tailrec

trait Removals extends Constraint {

  val removals = new Array[Int](arity)

  final override def setRemovals(p: Int, v: Int) {
    removals(p) = v
  }

  final override def fillRemovals(value: Int) {
    Arrays.fill(removals, value);
  }

  final override def revise(reviseCount: Int) {
    revise(modified(reviseCount))
  }

  def revise(modified: Seq[Int])

  /**
   * If only one variable in the scope has been altered, its revision can be skipped
   */
  final def skipRevision(modified: Seq[Int]) =
    modified.take(2) match {
      case Seq(c) => c
      case _ => -1
    }

  // scope.iterator.zipWithIndex.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

  private def modified(reviseCount: Int) = {
    def mod(i: Int): Stream[Int] =
      if (i < 0) Stream.empty
      else if (removals(i) >= reviseCount) i #:: mod(i - 1)
      else mod(i - 1)

    mod(arity - 1)
  }
  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}