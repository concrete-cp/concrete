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

package concrete

import concrete.constraint.extension.ExtensionConstraint
import concrete.constraint.Constraint
import concrete.priorityqueues.Identified
import concrete.priorityqueues.PTag
import scala.annotation.tailrec
import concrete.priorityqueues.DLNode

final class Variable(
  val name: String,
  val dom: Domain) extends Identified with PTag with DLNode[Variable] {
  require(dom ne null)

  var getId = -1

  private var _constraints: Array[Constraint] = Array.empty

  private var _extensionConstraints: List[ExtensionConstraint] = Nil

  private var _positionInConstraint: Array[Int] = Array.empty

  override def toString = s"$name $dom"

  def constraints = _constraints

  def addConstraint(newConstraint: Constraint) {
    _constraints :+= newConstraint
    if (newConstraint.isInstanceOf[ExtensionConstraint]) {
      _extensionConstraints ::= newConstraint.asInstanceOf[ExtensionConstraint]
    }
    _positionInConstraint :+= newConstraint.position(this)
    wDeg += newConstraint.weight
  }


  def dynamicConstraints = _extensionConstraints
  def positionInConstraint = _positionInConstraint

  def indices = dom.indices

  def values = dom.values

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

