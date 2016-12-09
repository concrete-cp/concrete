package concrete

import concrete.constraint.Constraint
import cspom.util.BitVector


object EntailmentManagerLight {
  def apply(variables: Seq[Variable]): EntailmentManagerLight =
    new EntailmentManagerLight(
      Vector(variables.map(v => BitVector.filled(v.constraints.length)): _*))
}

final class EntailmentManagerLight(
    val activeConstraints: Vector[BitVector]) extends AnyVal {

  def addConstraints(constraints: Seq[Constraint]): EntailmentManagerLight = {
    var ac = activeConstraints
    for (c <- constraints) {
      for (i <- c.scope.indices) {
        val vid = c.scope(i).id
        /* Fake variables (constants) may appear in constraints */
        if (vid >= 0) {
          ac = ac.updated(vid, ac(vid) + c.positionInVariable(i))
        }
      }
    }
    new EntailmentManagerLight(ac)
  }

  def entail(c: Constraint, ps: ProblemState): EntailmentManagerLight = {
    var ac = activeConstraints
    var i = c.arity - 1
    while (i >= 0) {
      val v = c.scope(i)
      val vid = v.id
      /* Fake variables (constants) may appear in constraints */

      if (vid >= 0) {
        val pos = c.positionInVariable(i)

        /* pos >= 0 is for reified constraints */
        if (pos >= 0 && !ps.assigned(v)) {
          ac = ac.updated(vid, ac(vid) - c.positionInVariable(i))
        }

      }
      i -= 1
    }
    new EntailmentManagerLight(ac)

  }

  def entail(c: Constraint, i: Int): EntailmentManagerLight = {
    val ac = activeConstraints

    val vid = c.scope(i).id
    /* Fake variables (constants) may appear in constraints */

    if (vid >= 0) {
      val pos = c.positionInVariable(i)

      /* For reified constraints */
      if (pos >= 0) {
        new EntailmentManagerLight(
          ac.updated(vid, ac(vid) - c.positionInVariable(i)))
      } else {
        this
      }

    } else {
      this
    }

  }

  def active(v: Variable): BitVector = {
    val vid = v.id
    activeConstraints(vid)

  }

  def apply(c: Constraint): Boolean = {
    (0 until c.arity).exists { i =>

      val v = c.scope(i)
      v.id >= 0 && {
        val pos = c.positionInVariable(i)
        pos >= 0 && !activeConstraints(c.scope(i).id)(c.positionInVariable(i))
      }

    }

  }

  // override def toString: String = modified.traversable.mkString("entailed: ", ", ", "")
}