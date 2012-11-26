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
import cspfj.priorityqueues.Identified
import cspfj.UNSATException
import cspfj.priorityqueues.PTag
import cspfj.Variable
import scala.collection.mutable.DoubleLinkedList
import cspfj.priorityqueues.DLNode

object Constraint {
  var cId = 0;
  def reset() { cId = 0 }
}

abstract class Constraint(val scope: Array[Variable])
  extends DLNode[Constraint] with Weighted with Identified with PTag {

  val getId = Constraint.cId
  Constraint.cId += 1

  private var entailedAtLevel = -1;

  /**
   * arity is the number of variables involved by the constraint
   */
  val arity = scope.size

  /**
   * @return the scope of the constraint as an unordered Set
   */
  val scopeSet = scope.toSet

  /**
   * @return a map containing the positions of variables in the scope of the constraint.
   */
  val position = scope.zipWithIndex.toMap

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

  @tailrec
  final def controlTuplePresence(tuple: Array[Int], i: Int = arity - 1): Boolean = {
    /** Need high optimization */

    if (i < 0) true
    else
      scope(i).dom.present(tuple(i)) && controlTuplePresence(tuple, i - 1)

  }

  @tailrec
  final def controlTuplePresence(tuple: Array[Int], mod: List[Int]): Boolean = {
    /** Need high optimization */

    if (mod eq Nil) {
      assert(controlTuplePresence(tuple), tuple.mkString("(", ", ", ")") +
        " is not in " + this)
      true
    } else {
      val i = mod.head
      scope(i).dom.present(tuple(i)) && controlTuplePresence(tuple, mod.tail)
    }

  }

  final def isEntailed = entailedAtLevel >= 0

  var inCN = false

  final def disEntail() {
    entailedAtLevel = -1
    if (inCN) for (v <- scope) {
      v.dDeg += 1 //weight
    }
  }

  final def entail() {
    if (!isEntailed) {
      entailedAtLevel = level
      if (inCN) for (v <- scope) {
        v.dDeg -= 1 //weight
      }
    }

  }

  override def weight_=(w: Int) {
    assert(!isEntailed)
    assert(inCN)
    //    for (v <- scope) {
    //      v.wDeg += w - weight
    //    }
    super.weight = w
  }

  //  def consistentRevise() = try {
  //    revise()
  //    true
  //  } catch {
  //    case e: UNSATException => false
  //  }

  def isConsistent(): Boolean = {
    setLvl(level + 1)
    scope foreach { _.dom.setLevel(level) }
    try {
      revise()
      true
    } catch {
      case e: UNSATException => false
    } finally {
      restoreLvl(level - 1)
      scope foreach { _.dom.restoreLevel(level) }
    }
  }

  def advise(pos: Int): Int

  def advise(v: Variable): Int = advise(position(v))

  /**
   * The constraint propagator. Returns true if any domain change was done.
   *
   * @param revisator
   */
  @throws(classOf[UNSATException])
  def revise(): Boolean

  private var valTuple = new Array[Int](arity)

  /**
   * @return true iff the constraint is satisfied by the given tuple
   */
  def checkIndices(tuple: Array[Int]) = {
    var i = arity - 1
    while (i >= 0) {
      valTuple(i) = scope(i).dom.value(tuple(i))
      i -= 1
    }
    checkValues(valTuple)
  }

  def checkValues(tuple: Array[Int]): Boolean

  def simpleEvaluation: Int

  override def toString = this.getClass.getSimpleName + scope.mkString("(", ", ", ")")

  /**
   * @param variable
   * @return true iff the given variable is involved by the constraint
   */
  final def isInvolved(variable: Variable) = position.contains(variable)

  @tailrec
  final def sizes(a: Array[Int] = new Array[Int](arity), i: Int = arity - 1): Array[Int] =
    if (i < 0) a
    else {
      a(i) = scope(i).dom.size
      sizes(a, i - 1)
    }

  def isBound = scope.forall(_.dom.bound)

  final def cardSize: Int = {
    var size = 1
    for (v <- scope) {
      if (size > Int.MaxValue / v.dom.size) {
        return -1;
      }
      size *= v.dom.size
    }
    size
  }

  final def hasChanged[A](l: Traversable[A], f: A => Boolean) = {
    var ch = false
    for (e <- l) {
      ch |= f(e)
    }
    ch
  }

}
