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

import java.util.Collection;

import java.util.List;

import com.google.common.base.Predicates;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;

import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.priorityqueues.Identified;
import cspfj.util.BitVector;

object Variable {
  var nbV = 0;

  /**
   * Reinit ID generator (before loading a new problem).
   */
  def resetVId() { nbV = 0 }
}

final class Variable(
  val name: String,
  private var _domain: Domain) extends Identified {

  val getId = Variable.nbV
  Variable.nbV += 1

  private var _constraints: IndexedSeq[Constraint] = null

  private var _dynamicConstraints: Seq[DynamicConstraint] = null

  private var _positionInConstraint: IndexedSeq[Int] = null

  override def toString = name + (if (_domain == null) " [?]" else " " + _domain)

  def constraints = _constraints

  /**
   * @param newConstraints
   *            Liste des contraintes impliquant la variable
   */
  def constraints_=(newConstraints: Seq[Constraint]) {
    _constraints = newConstraints.toIndexedSeq
    _dynamicConstraints = newConstraints filter { _.isInstanceOf[DynamicConstraint] } map { _.asInstanceOf[DynamicConstraint] }
    _positionInConstraint = constraints.map(_.getPosition(this))
  }

  def domain = _domain

  def domain_=(d: Domain) {
    require(_domain == null, "Reassignment of domain")
    _domain = d
  }

  def dynamicConstraints = _dynamicConstraints
  def positionInConstraint = _positionInConstraint

  def currentIndexes = if (_domain == null) null else _domain.currentIndexes

  def currentValues = if (_domain == null) null else _domain.currentValues
}
