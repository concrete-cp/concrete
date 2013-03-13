package cspfj.constraint

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import cspfj.Variable
import scala.collection.mutable.HashSet

trait Shaver extends VariablePerVariable {

  def shave(): List[Int]

  final override def revise() = {
    if (isBound) {
      val c = shave()
      assert({ (0 until arity) foreach advise; super.revise().isEmpty }, this + " is not BC")
      entailCheck()
      c
    } else {
      /* Shaving may exhibit holes, in this case, going for a fixpoint is a must */
      val c = fixPoint(shave())
      if (c.isEmpty) {
        super.revise()
      } else {
        /* Shaving may have restored bound consistency */
        if (isBound) {
          c
        } else {
          c.foreach(advise)
          c ++= super.revise()
        }
        c
      }
    }
  }

  @tailrec
  private def fixPoint(f: => List[Int], ch: HashSet[Int] = new HashSet()): HashSet[Int] = {
    val c = f
    if (c.isEmpty) {
      ch
    } else {
      ch ++= c
      fixPoint(f, ch)
    }
  }

}