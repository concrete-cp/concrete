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

package cspfj.constraint;

import cspfj.constraint.extension.TupleManager;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

object Constraint {
  var cId = 0;
  def reset() { cId = 0 }

  var nbPresenceChecks = 0

  def clearStats() { nbPresenceChecks = 0 }
}

object NULL_REVISATOR extends RevisionHandler {
  def revised(constraint: Constraint, variable: Variable) {}
}

abstract class AbstractConstraint(
  _name: String,
  val scope: IndexedSeq[Variable]) extends Constraint {
  val name = if (_name == null) "C" + getId else _name
  val arity = scope.size
  val scopeSet = scope.toSet
  val tuple = new Array[Int](arity)
  val tupleManager = new TupleManager(this, tuple)
  val position = scope.zipWithIndex.map { case (value, index) => value -> index }.toMap
  def this(scope: Variable*) = this(null, scope.toIndexedSeq)
  def isInvolved(variable: Variable) = position.contains(variable)
}

trait Weighted {
  var weight = 1
}

trait Constraint extends Weighted {
  def scope: IndexedSeq[Variable];

  val getId = Constraint.cId
  Constraint.cId += 1

  def arity: Int
  def name: String

  private var entailedAtLevel = -1;

  private var removals = 0;

  protected def tuple: Array[Int]

  def position: Map[Variable, Int]

  def getValue(position: Int) = scope(position).domain.value(tuple(position))

  def isInvolved(variable: Variable): Boolean

  def scopeSet: Set[Variable]

  override def equals(o: Any) = o.asInstanceOf[Constraint].getId == getId

  def getType = getClass.getSimpleName

  private var _level = 0;

  def level = _level
  def level_=(l: Int) {
    if (entailedAtLevel > level) {
      // LOGGER.finest("Disentailing " + this);
      disEntail();
    }
    _level = l;
  }

  override def hashCode = getId

  def controlTuplePresence(tuple: Array[Int]) = {
    Constraint.nbPresenceChecks += 1;
    scope.zip(tuple).forall(vv => vv._1.domain.present(vv._2))
  }

  def getRemovals(position: Int): Int = throw new UnsupportedOperationException

  def setRemovals(position: Int, value: Int) { removals = value }

  def fillRemovals(value: Int) { removals = value }

  def hasNoRemovals(reviseCount: Int) = removals < reviseCount

  final def isEntailed = entailedAtLevel >= 0

  final def disEntail() { entailedAtLevel = -1 }

  final def entail() { entailedAtLevel = level }

  def isConsistent(reviseCount: Int) = {
    level += 1
    scope foreach { _.domain.setLevel(level) }
    val consistent = revise(NULL_REVISATOR, reviseCount)
    level -= 1
    scope foreach { _.domain.setLevel(level) }
    consistent
  }

  def revise(revisator: RevisionHandler, reviseCount: Int): Boolean
  def check: Boolean
}
