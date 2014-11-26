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

package concrete.constraint.semantic;

import concrete.Domain
import concrete.Revised
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Stateless

final class GtC(val v: Variable, val constant: Int) extends Constraint(Array(v)) with Stateless {
  def check(t: Array[Int]) = t(0) > constant
  def advise(domains: IndexedSeq[Domain], p: Int) = 1
  def revise(domains: IndexedSeq[Domain]) = {
    Revised(Vector(domains(0).removeTo(constant)), true)
  }
  def simpleEvaluation = 1
}

final class LtC(val v: Variable, val constant: Int) extends Constraint(Array(v)) with Stateless {
  def check(t: Array[Int]) = t(0) < constant
  def advise(domains: IndexedSeq[Domain], p: Int) = 1
  def revise(domains: IndexedSeq[Domain]) = {
    Revised(Vector(domains(0).removeFrom(constant)), true)
  }
  def simpleEvaluation = 1
}

/**
 * Constraint v0 + constant >(=) v1
 */
final class Gt(val v0: Variable, val constant: Int, val v1: Variable, val strict: Boolean)
  extends Constraint(Array(v0, v1)) with Stateless {

  def this(v0: Variable, v1: Variable, strict: Boolean) =
    this(v0, 0, v1, strict);

  override def check(t: Array[Int]) =
    if (strict) t(0) + constant > t(1);
    else t(0) + constant >= t(1);

  def revise(domains: IndexedSeq[Domain]) = {

    if (strict) {
      val d0 = domains(0).removeTo(domains(1).head - constant)
      val d1 = domains(1).removeFrom(domains(0).last + constant)
      Revised(Vector(d0, d1), d1.last < d0.head + constant)
    } else {
      val d0 = domains(0).removeUntil(domains(1).head - constant)
      val d1 = domains(1).removeAfter(domains(0).last + constant)
      Revised(Vector(d0, d1), d1.last <= d0.head + constant)
    }

  }

  override def isConsistent(domains: IndexedSeq[Domain]) = {
    val max0 = domains(0).last + constant;
    val min1 = domains(1).head
    max0 > min1 || !strict && max0 == min1;
  }

  override def toString(domains: IndexedSeq[Domain]) = domains(0).toString + (
    if (constant > 0)
      " + " + constant
    else if (constant < 0)
      " - " + (-constant)
    else "") + (if (strict) " > " else " >= ") + domains(1)

  def advise(domains: IndexedSeq[Domain], p: Int) = 2

  val simpleEvaluation = 1
}
