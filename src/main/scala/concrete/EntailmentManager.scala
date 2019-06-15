package concrete

import bitvectors.BitVector
import concrete.constraint.Constraint

object EntailmentManager {
  def apply(variables: Seq[Variable]): EntailmentManager =
    new EntailmentManager(
      Vector(variables.map(v => BitVector.filled(v.constraints.length)): _*),
      Set(),
      Vector.fill(variables.length)(0)
    )
}

final class EntailmentManager(
                                    val activeConstraints: Vector[BitVector],
                                    val entailedReified: Set[Int],
                                    val wDegsMinus: Vector[Int]) {

  def addConstraints(constraints: Iterable[Constraint]): EntailmentManager = {
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
    new EntailmentManager(ac, entailedReified, wDegsMinus)
  }

  def wDeg(v: Variable): Int = v.weight - wDegsMinus(v.id)

  def entail(c: Constraint, ps: ProblemState): EntailmentManager = {

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
          return new EntailmentManager(ac, entailedReified + c.id, wd)
        } else if (!ps.dom(v).isAssigned) {
          ac = ac.updated(vid, ac(vid) - c.positionInVariable(i))
          wd = wd.updated(vid, wDegsMinus(vid) + c.weight)
        }

      }
      i -= 1
    }
    new EntailmentManager(ac, entailedReified, wd)

  }

  def entail(c: Constraint, i: Int): EntailmentManager = {
    val vid = c.scope(i).id
    /* Fake variables (constants) may appear in constraints */

    if (vid >= 0) {
      val pos = c.positionInVariable(i)

      /* For reified constraints */
      if (pos < 0) {
        new EntailmentManager(activeConstraints, entailedReified + c.id, wDegsMinus)
      } else {
        new EntailmentManager(
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

  def entailedReif(c: Constraint): Boolean = entailedReified(c.id)

  def hasInactiveVar(c: Constraint): Boolean = {
    (0 until c.arity).exists { i =>

      val v = c.scope(i)
      v.id >= 0 && {
        val pos = c.positionInVariable(i)
        pos >= 0 && !activeConstraints(v.id).contains(pos)
      }

    }

  }

  // override def toString: String = modified.traversable.mkString("entailed: ", ", ", "")
}