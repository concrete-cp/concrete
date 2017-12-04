/**
  * CSPFJ - CSP solving API for Java
  * Copyright (C) 2006 Julien VION
  *
  * This library is free software; you can redistribute it and/or
  * modify it under the terms of the GNU Lesser General Public
  * License as published by the Free Software Foundation; either
  * version 2.1 of the License, or (at your option) any later version.
  *
  * This library is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  * Lesser General Public License for more details.
  *
  * You should have received a copy of the GNU Lesser General Public
  * License along with this library; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
  */

package concrete.constraint.linear

;

import bitvectors.BitVector
import concrete._
import concrete.constraint.Constraint

final class LeC(val v: Variable, val constant: Int) extends Constraint(Array(v)) {
  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) <= constant

  def advise(ps: ProblemState, event: Event, p: Int) = 1

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome = if (ps.dom(v).head <= constant) ps else Contradiction(scope)

  def revise(ps: ProblemState, mod: BitVector): Outcome = ps.removeAfter(v, constant).entail(this)

  def simpleEvaluation = 1

  override def toString(ps: ProblemState) = s"${v.toString(ps)} <= $constant"
}

final class LtC(val v: Variable, var constant: Int) extends Constraint(Array(v)) {
  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) < constant

  def advise(ps: ProblemState, event: Event, p: Int) = 1

  def revise(ps: ProblemState, mod: BitVector): Outcome = ps.removeFrom(v, constant) //.entail(this)
  def simpleEvaluation = 1

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome = if (ps.dom(v).head < constant) ps else Contradiction(scope)

  override def toString(ps: ProblemState) = s"${v.toString(ps)} < $constant"
}

final class GeC(val v: Variable, val constant: Int) extends Constraint(Array(v)) {
  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) >= constant

  def advise(ps: ProblemState, event: Event, p: Int) = 1

  def revise(ps: ProblemState, mod: BitVector): Outcome = ps.removeUntil(v, constant).entail(this)

  def simpleEvaluation = 1

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome = if (ps.dom(v).last >= constant) ps else Contradiction(scope)

  override def toString(ps: ProblemState) = s"${v.toString(ps)} >= $constant"
}

final class GtC(val v: Variable, var constant: Int) extends Constraint(Array(v)) {
  def init(ps: ProblemState): ProblemState = ps

  def check(t: Array[Int]): Boolean = t(0) > constant

  def advise(ps: ProblemState, event: Event, p: Int) = 1

  def revise(ps: ProblemState, mod: BitVector): Outcome = ps.removeTo(v, constant) //.entail(this)
  def simpleEvaluation = 1

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome = if (ps.dom(v).last > constant) ps else Contradiction(scope)

  override def toString(ps: ProblemState) = s"${v.toString(ps)} > $constant"
}

/**
  * Constraint v0 + constant >(=) v1
  */
final class Gt(val v0: Variable, val constant: Int, val v1: Variable, val strict: Boolean)
  extends Constraint(Array(v0, v1)) {

  val simpleEvaluation = 1

  def init(ps: ProblemState): ProblemState = {
    ps
  }

  def this(v0: Variable, v1: Variable, strict: Boolean) =
    this(v0, 0, v1, strict)

  override def check(t: Array[Int]): Boolean = {
    if (strict) {
      t(0) + constant > t(1)
    } else {
      t(0) + constant >= t(1)
    }
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    if (strict) {
      ps
        .removeTo(v0, ps.dom(v1).head - constant)
        .removeFrom(v1, ps.dom(v0).last + constant)
        .entailIf(this, mod =>
          mod.dom(v1).last < mod.dom(v0).head + constant)
    } else {
      ps
        .removeUntil(v0, ps.dom(v1).head - constant)
        .removeAfter(v1, ps.dom(v0).last + constant)
        .entailIf(this, mod =>
          mod.dom(v1).last <= mod.dom(v0).head + constant)
    }

  }

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome = {
    val max0 = ps.dom(v0).last + constant
    val min1 = ps.dom(v1).head

    if (max0 > min1 || !strict && max0 == min1) ps else Contradiction(scope)
  }

  override def toString(ps: ProblemState) =
    s"${v0.toString(ps)} ${
      if (constant > 0) {
        " + " + constant
      } else if (constant < 0) {
        " - " + (-constant)
      } else {
        ""
      }
    } ${if (strict) " > " else " >= "} ${v1.toString(ps)}"

  def advise(ps: ProblemState, event: Event, p: Int): Int = if (event <= BoundRemoval) 2 else -1
}
