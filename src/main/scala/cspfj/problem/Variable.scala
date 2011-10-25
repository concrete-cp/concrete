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

package cspfj.problem;

import cspfj.constraint.Constraint
import cspfj.constraint.DynamicConstraint
import cspfj.priorityqueues.Identified

final class Variable(
  val name: String,
  private var _domain: Domain) extends Identified {

  val getId = Variable.nbV
  Variable.nbV += 1

  private var _constraints: IndexedSeq[Constraint] = IndexedSeq.empty

  private var _dynamicConstraints: Seq[DynamicConstraint] = Nil

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
    if (newConstraint.isInstanceOf[DynamicConstraint]) {
      _dynamicConstraints :+= newConstraint.asInstanceOf[DynamicConstraint]
    }
    _positionInConstraint :+= newConstraint.position(this)
  }
  
  def dom = _domain

  def dom_=(d: Domain) {
    require(_domain == null, "Reassignment of domain")
    _domain = d
  }

  def dynamicConstraints = _dynamicConstraints
  def positionInConstraint = _positionInConstraint

  def indices = if (_domain == null) null else _domain.indices

  def values = if (_domain == null) null else _domain.values
}

object Variable {
  var nbV = 0;

  /**
   * Reinit ID generator (before loading a new problem).
   */
  def resetVId() { nbV = 0 }
}