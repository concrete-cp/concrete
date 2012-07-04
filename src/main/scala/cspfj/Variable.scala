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

  def indices = if (_domain == null) null else _domain.indices

  def values = if (_domain == null) null else _domain.values

  //var wDeg = 0

  @tailrec
  private def _getWDeg(i: Int = constraints.size - 1, s: Int = 0): Int =
    if (i < 0) s
    else {
      val c = constraints(i)
      if (c.isEntailed) _getWDeg(i - 1, s)
      else _getWDeg(i - 1, s + c.weight)
    }

  def getWDeg = {
    _getWDeg()
    //    assert(_getWDeg() == wDeg, "%s: was %d, should be %d".format(this, wDeg, _getWDeg()))
    //    
    //    wDeg
  }

  var dDeg = 0

  @tailrec
  private def _getDDeg(i: Int = constraints.size - 1, s: Int = 0): Int =
    if (i < 0) s
    else {
      val c = constraints(i)
      if (c.isEntailed) _getDDeg(i - 1, s)
      else _getDDeg(i - 1, s + 1)
    }

  def getDDeg = dDeg

}

object Variable {
  var nbV = 0;

  /**
   * Reinit ID generator (before loading a new problem).
   */
  def resetVId() { nbV = 0 }
}