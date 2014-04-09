package concrete.constraint

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

trait BC extends Constraint {
  def shave(): Seq[Int]

  override def revise() = {
    val c = shave()

    if (intervalsOnly) {
      c
    } else {
      fixPoint(shave, c.toSet)
    }
  }

  @tailrec
  private def fixPoint(f: => Seq[Int], ch: Set[Int]): Set[Int] = {
    val c = f
    if (c.isEmpty) {
      ch
    } else {
      fixPoint(f, ch ++ c)
    }
  }
}
