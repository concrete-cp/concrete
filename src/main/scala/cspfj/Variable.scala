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

package cspfj

import cspfj.constraint.extension.ExtensionConstraint
import cspfj.constraint.Constraint
import cspfj.priorityqueues.Identified
import cspfj.priorityqueues.PTag
import scala.annotation.tailrec
import cspfj.priorityqueues.DLNode

final class Variable(
  val name: String,
  private var _domain: Domain) extends Identified with PTag with DLNode[Variable] {

  val getId = Variable.nbV
  Variable.nbV += 1

  private var _constraints: Array[Constraint] = Array.empty

  private var _extensionConstraints: List[ExtensionConstraint] = Nil

  private var _positionInConstraint: Array[Int] = Array.empty

  override def toString = name + (if (_domain == null) " [?]" else " " + _domain)

  def constraints = _constraints

  //  /**
  //   * @param newConstraints
  //   *            Liste des contraintes impliquant la variable
  //   */
  //  def constraints_=(newConstraints: Seq[Constraint]) {
  //    _constraints = newConstraints.toIndexedSeq
  //    _dynamicConstraints = newConstraints filter { _.isInstanceOf[DynamicConstraint] } map { _.asInstanceOf[DynamicConstraint] }
  //    _positionInConstraint = constraints.map(_.position(this)).toArray
  //  }

  def addConstraint(newConstraint: Constraint) {
    _constraints :+= newConstraint
    if (newConstraint.isInstanceOf[ExtensionConstraint]) {
      _extensionConstraints ::= newConstraint.asInstanceOf[ExtensionConstraint]
    }
    _positionInConstraint :+= newConstraint.position(this)
    dDeg += 1 //newConstraint.weight
  }

  def dom = _domain

  def dom_=(d: Domain) {
    require(_domain == null, "Reassignment of domain")
    _domain = d
  }

  def dynamicConstraints = _extensionConstraints
  def positionInConstraint = _positionInConstraint

  def indices = _domain.indices

  def values = _domain.values

  //var wDeg = 0

  //  @tailrec
  //  private def _getWDeg(i: Int = constraints.size - 1, s: Int = 0): Int =
  //    if (i < 0) s
  //    else {
  //      val c = constraints(i)
  //      if (c.isEntailed) _getWDeg(i - 1, s)
  //      else _getWDeg(i - 1, s + c.weight)
  //    }

  private def nonFree(c: Constraint): Boolean = {
    var free = 0
    var i = c.arity - 1
    while (i >= 0) {
      if (c.scope(i).dom.size > 1) {
        free += 1
        if (free > 1) {
          return true
        }
      }
      i -= 1
    }
    false
  }

  def getWDegFree = {
    var i = constraints.length - 1
    var wDeg = 0
    while (i >= 0) {
      val c = constraints(i)
      if (nonFree(c)) wDeg += c.weight
      i -= 1
    }
    wDeg
  }

  def getWDegEntailed = {
    var i = constraints.length - 1
    var wDeg = 0
    while (i >= 0) {
      val c = constraints(i)
      if (!c.isEntailed) wDeg += c.weight
      i -= 1
    }
    wDeg
  }

  var dDeg = 0

  def getDDeg = dDeg

  def getDDegFree = {
    var i = constraints.length - 1
    var dDeg = 0
    while (i >= 0) {
      val c = constraints(i)
      if (nonFree(c)) dDeg += 1
      i -= 1
    }
    dDeg
  }

}

object Variable {
  var nbV = 0;

  /**
   * Reinit ID generator (before loading a new problem).
   */
  def resetVId() { nbV = 0 }
}
