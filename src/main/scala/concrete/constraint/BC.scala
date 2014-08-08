package concrete.constraint

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

trait BC extends Constraint {
  def shave(): Seq[Int]

  override def revise(): Traversable[Int] = {
    val ch = collection.mutable.Set[Int]()

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
