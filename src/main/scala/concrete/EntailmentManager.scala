package concrete

import bitvectors.BitVector
import concrete.constraint.Constraint

object EntailmentManagerLight {
  def apply(variables: Seq[Variable]): EntailmentManagerLight =
    new EntailmentManagerLight(
      Vector(variables.map(v => BitVector.filled(v.constraints.length)): _*),
      Set(),
      Vector.fill(variables.length)(0)
    )
}

final class EntailmentManagerLight(
                                    val activeConstraints: Vector[BitVector],
                                    val entailedReified: Set[Int],
                                    val wDegsMinus: Vector[Int]) {

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
    new EntailmentManagerLight(ac, entailedReified, wDegsMinus)
  }

  def wDeg(v: Variable): Int = v.weight - wDegsMinus(v.id)

  def entail(c: Constraint, ps: ProblemState): EntailmentManagerLight = {

    var ac = activeConstraints
    var wd = wDegsMinus

    var i = c.arity - 1

    while (i >= 0) {
      val v = c.scope(i)
      val vid = v.id
      /* Fake variables (constants) may appear in constraints */

      if (vid >= 0) {
        val pos = c.positionInVariable(i)

        if (pos < 0) {
          /* case of reified constraints */
          assert(ac eq activeConstraints, "one variable is not registered")
          return new EntailmentManagerLight(ac, entailedReified + c.id, wd)
        } else if (!ps.dom(v).isAssigned) {
          ac = ac.updated(vid, ac(vid) - c.positionInVariable(i))
          wd = wd.updated(vid, wDegsMinus(vid) + c.weight)
        }

      }
      i -= 1
    }
    new EntailmentManagerLight(ac, entailedReified, wd)

  }

  def entail(c: Constraint, i: Int): EntailmentManagerLight = {
    val vid = c.scope(i).id
    /* Fake variables (constants) may appear in constraints */

    if (vid >= 0) {
      val pos = c.positionInVariable(i)

      /* For reified constraints */
      if (pos < 0) {
        new EntailmentManagerLight(activeConstraints, entailedReified + c.id, wDegsMinus)
      } else {
        new EntailmentManagerLight(
          activeConstraints.updated(vid, activeConstraints(vid) - c.positionInVariable(i)),
          entailedReified,
          wDegsMinus.updated(vid, wDegsMinus(vid) + c.weight)
        )
      }

    } else {
      this
    }

  }

  def active(v: Variable): BitVector = {
    val vid = v.id
    activeConstraints(vid)
  }

  def entailedReif(c: Constraint) = entailedReified(c.id)

  def hasInactiveVar(c: Constraint) = {
    (0 until c.arity).exists { i =>

      val v = c.scope(i)
      v.id >= 0 && {
        val pos = c.positionInVariable(i)
        pos >= 0 && !activeConstraints(v.id)(pos)
      }

    }

  }

  // override def toString: String = modified.traversable.mkString("entailed: ", ", ", "")
}