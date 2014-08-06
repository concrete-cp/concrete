package concrete.constraint

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

trait BC extends Constraint {
  def shave(): Seq[Int]

  override def revise() = {
    if (intervalsOnly) {
      shave()
    } else {
      fixPoint()
    }
  }

  @tailrec
  private def fixPoint(ch: Set[Int] = Set()): Set[Int] = {
    val c = shave()
    if (c.isEmpty) {
      ch
    } else {
      fixPoint(ch ++ c)
    }
  }
}
