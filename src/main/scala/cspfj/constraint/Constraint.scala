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

import cspfj.constraint.extension.TupleManager
import cspfj.filter.RevisionHandler
import cspfj.problem.Variable;
import cspfj.heuristic.Weighted

object Constraint {
  var cId = 0;
  def reset() { cId = 0 }

  var nbPresenceChecks = 0

  def clearStats() { nbPresenceChecks = 0 }
}

object NULL_REVISATOR extends RevisionHandler {
  def revised(constraint: Constraint, variable: Variable) {}
}

trait Constraint extends Weighted {

  val getId = Constraint.cId
  Constraint.cId += 1

  /**
   * arity is the number of variables involved by the constraint
   */
  def arity: Int
  def name: String

  private var entailedAtLevel = -1;

  private var removals = 0;

  protected def tuple: Array[Int]

  /**
   * @return a map containing the positions of variables in the scope of the constraint.
   */
  def position: Map[Variable, Int]

  def getValue(position: Int) = scope(position).domain.value(tuple(position))

  /**
   * @param variable
   * @return true iff the given variable is involved by the constraint
   */
  def isInvolved(variable: Variable): Boolean

  /**
   * @return the scope of the constraint
   */
  def scope: Array[Variable];

  /**
   * @return the scope of the constraint as an unordered Set
   */
  def scopeSet: Set[Variable]

  override def equals(o: Any) = o.asInstanceOf[Constraint].getId == getId

  /**
   * @return string description of the constraint
   */
  def getType = getClass.getSimpleName

  private var _level = 0;

  def level = _level
  def level_=(l: Int) {
    if (entailedAtLevel > l) {
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

  /**
   * The constraint propagator.
   *
   * @param revisator
   * @return false iff an inconsistency has been detected
   */
  def revise(revisator: RevisionHandler, reviseCount: Int): Boolean

  /**
   * @return true iff the constraint is satisfied by the current values of the
   *         tuple associated to the constraint object (see tuple: Array[Int])
   */
  def check: Boolean

  /**
   * @return true iff the constraint is satisfied by the given tuple
   */
  def check(tuple: Array[Int]): Boolean = {
    val current = this.tuple
    tuple.copyToArray(this.tuple)
    val c = this.check
    current.copyToArray(this.tuple)
    c
  }
}
