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
  require(_domain ne null)

  val getId = Variable.nbV
  Variable.nbV += 1

  private var _constraints: Array[Constraint] = Array.empty

  private var _extensionConstraints: List[ExtensionConstraint] = Nil

  private var _positionInConstraint: Array[Int] = Array.empty

  override def toString = s"$name ${_domain}"

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
    wDeg += newConstraint.weight
  }

  def dom = _domain

  def dom_=(d: Domain) {
    require(d ne null)
    require(_domain eq UndefinedDomain, "Reassignment of domain")
    _domain = d
  }

  def dynamicConstraints = _extensionConstraints
  def positionInConstraint = _positionInConstraint

  def indices = _domain.indices

  def values = _domain.values

  var wDeg = 0

  def getWDegFree = {
    var i = constraints.length - 1
    var wDeg = 0
    while (i >= 0) {
      val c = constraints(i)
      if (!c.isFree) wDeg += c.weight
      i -= 1
    }
    wDeg
  }

  def getWDegEntailed = {
    this.wDeg
    //    var i = constraints.length - 1
    //    var wDeg = 0
    //    while (i >= 0) {
    //      val c = constraints(i)
    //      if (!c.isEntailed) wDeg += c.weight
    //      i -= 1
    //    }
    //    wDeg
  }

  //var dDeg = 0

  def getDDegEntailed = {
    var i = constraints.length - 1
    var dDeg = 0
    while (i >= 0) {
      if (!constraints(i).isEntailed) dDeg += 1
      i -= 1
    }
    dDeg
  }

  def getDDegFree = {
    var i = constraints.length - 1
    var dDeg = 0
    while (i >= 0) {
      if (!constraints(i).isFree) dDeg += 1
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
