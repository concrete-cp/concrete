package cspfj.constraint

import scala.annotation.tailrec

trait Shaver extends VariablePerVariable {

  def shave(): Boolean

  final override def revise() = {
    if (isBound) {
      val c = shave()
      assert({ adviseAll(); !super.revise() }, this + " is not BC")
      entailCheck(c)
      c
    } else {
      /* Shaving may exhibit holes, in this case, going for a fixpoint is a must */
      val s = sizes()
      val c = fixPoint(shave())
      if (c) {
        /* Shaving may have restored bound consistency */
        if (!isBound) {
          for (p <- 0 until arity if s(p) != scope(p).dom.size) advise(p)
          super.revise()
        }
        true
      } else super.revise()
    }
  }

  @tailrec
  private def fixPoint(f: => Boolean, ch: Boolean = false): Boolean =
    if (f) fixPoint(f, true) else ch

}