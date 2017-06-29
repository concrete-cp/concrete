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

import concrete.constraint.Constraint
import concrete.constraint.extension.ExtensionConstraint
import concrete.heuristic.Weighted
import concrete.priorityqueues.DLNode
import concrete.priorityqueues.Identified
import concrete.priorityqueues.PTag

final class Variable(
    val name: String,
    val initDomain: Domain) extends Identified with PTag with DLNode[Variable] with Weighted {

  var id = -1

  private var _constraints: Array[Constraint] = Array.empty

  private var _extensionConstraints: List[ExtensionConstraint] = Nil

  private var _positionInConstraint: Array[Array[Int]] = Array.empty

  override def toString = name

  def toString(ps: ProblemState) = s"$name ${ps.dom(this)}"

  def constraints = _constraints

  def addConstraint(newConstraint: Constraint): Int = {
    val existing = _constraints.indexOf(newConstraint)
    if (existing < 0) {

      val position = _constraints.length
      _constraints :+= newConstraint

      if (newConstraint.isInstanceOf[ExtensionConstraint]) {
        _extensionConstraints ::= newConstraint.asInstanceOf[ExtensionConstraint]
      }
      _positionInConstraint :+= newConstraint.position(this)
      position
    } else {
      existing
    }
  }

  def dynamicConstraints = _extensionConstraints
  def positionInConstraint(constraintPosition: Int) = _positionInConstraint(constraintPosition)

//  def getWDegEntailed(state: ProblemState) = {
//
//    val ac = state.activeConstraints(this)
//    var wDeg = 0
//    var i = ac.nextSetBit(0)
//    while (i >= 0) {
//      wDeg += constraints(i).weight
//      i = ac.nextSetBit(i + 1)
//    }
//    wDeg
//  }

  //var dDeg = 0

  def getDDegEntailed(state: ProblemState) = {
    state.activeConstraints(this).cardinality
  }

}

