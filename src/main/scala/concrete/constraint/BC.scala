package concrete.constraint

import scala.annotation.tailrec

trait BC extends Constraint {
  def shave(): Seq[Int]

  override def revise(): Traversable[Int] = {
    val ch = collection.mutable.BitSet()

    while (true) {
      val c = shave()
      if (c.isEmpty) {
        return ch
      } else {
        ch ++= c
      }
    }

    throw new IllegalStateException()
  }

}
