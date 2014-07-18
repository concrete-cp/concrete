package concrete.constraint

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.immutable.BitSet

trait BC extends Constraint {
  def shave(): Seq[Int]

  override def revise() = {
    if (intervalsOnly) {
      shave()
    } else {
      fixPoint(shave, BitSet())
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
