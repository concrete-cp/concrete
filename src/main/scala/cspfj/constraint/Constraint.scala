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

import scala.annotation.tailrec
import cspfj.heuristic.Weighted
import cspfj.priorityqueues.IOBinomialHeapNode
import cspfj.priorityqueues.Identified
import cspfj.problem.Variable
import cspfj.problem.EmptyDomainException
import cspfj.UNSATException
import cspfj.UNSATException
import cspfj.UNSATException
//import cspfj.priorityqueues.IOBinomialHeapNode

object Constraint {
  var cId = 0;
  def reset() { cId = 0 }

  var nbPresenceChecks = 0

  def clearStats() { nbPresenceChecks = 0 }
}

trait Constraint extends Weighted with Identified with IOBinomialHeapNode[Constraint] {

  val getId = Constraint.cId
  Constraint.cId += 1

  /**
   * arity is the number of variables involved by the constraint
   */
  def arity: Int
  def name: String

  private var entailedAtLevel = -1;

  def tuple: Array[Int]

  /**
   * @return a map containing the positions of variables in the scope of the constraint.
   */
  def position: Map[Variable, Int]

  def value(position: Int) = scope(position).dom.value(tuple(position))

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

  def setLvl(l: Int) {
    _level = l
  }

  def restoreLvl(l: Int) {
    if (entailedAtLevel > l) {
      // LOGGER.finest("Disentailing " + this);
      disEntail();
    }
    _level = l;
  }

  override def hashCode = getId

  final def controlTuplePresence(tuple: Array[Int]) = {
    Constraint.nbPresenceChecks += 1;
    /** Need high optimization */

    @tailrec
    def control(i: Int): Boolean =
      if (i < 0) true
      else
        scope(i).dom.present(tuple(i)) && control(i - 1)

    control(arity - 1)
  }

  final def isEntailed = entailedAtLevel >= 0

  final def disEntail() { entailedAtLevel = -1 }

  final def entail() { entailedAtLevel = level }

  def consistentRevise(reviseCount: Int) = try {
    revise(reviseCount)
    true
  } catch {
    case e: UNSATException => false
  }

  def isConsistent(reviseCount: Int): Boolean = {
    setLvl(level + 1)
    scope foreach { _.dom.setLevel(level) }
    try {
      revise(reviseCount)
      true
    } catch {
      case e: UNSATException => false
    } finally {
      restoreLvl(level - 1)
      scope foreach { _.dom.restoreLevel(level) }
    }
  }

  def fillRemovals(i: Int) {}

  def setRemovals(pos: Int, i: Int) {}

  /**
   * The constraint propagator.
   *
   * @param revisator
   */
  @throws(classOf[UNSATException])
  def revise(reviseCount: Int)

  /**
   * @return true iff the constraint is satisfied by the current values of the
   *         tuple associated to the constraint object (see tuple: Array[Int])
   */
  def check: Boolean

  /**
   * @return true iff the constraint is satisfied by the given tuple
   */
  def check(tuple: Array[Int]): Boolean = {
    val current = this.tuple.clone
    tuple.copyToArray(this.tuple)
    val c = this.check
    current.copyToArray(this.tuple)
    c
  }

  def getEvaluation: Int

  //def tupleManager: TupleManager

  def tupleValues = (0 to arity).iterator.map(p => scope(p).dom.value(tuple(p)))

  def sizes: Array[Int]

  override def toString = this.getClass.getName + scope.mkString("(", ", ", ")")

}
