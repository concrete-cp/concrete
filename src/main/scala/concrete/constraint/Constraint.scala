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

package concrete.constraint;

import scala.annotation.tailrec
import concrete.heuristic.Weighted
import concrete.priorityqueues.Identified
import concrete.UNSATException
import concrete.priorityqueues.PTag
import concrete.Variable
import scala.collection.mutable.DoubleLinkedList
import concrete.priorityqueues.DLNode

object Constraint {
  var cId = 0;
  def reset() { cId = 0 }

  val UNARY = 1
  val BINARY = 2
  val TERNARY = 3
  val NP = 7
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

    i < 0 || (scope(i).dom.present(tuple(i)) && controlTuplePresence(tuple, i - 1))

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
    assert(isEntailed)
    entailedAtLevel = -1
    if (inCN) for (v <- scope) {
      v.wDeg += weight
    }
  }

  final def entail() {
    if (!isEntailed) {
      entailedAtLevel = level
      if (inCN) for (v <- scope) {
        v.wDeg -= weight
      }
    }

  }

  override def weight_=(w: Int) {
    assert(!isEntailed)
    assert(inCN)
    val inc = w - weight
    for (v <- scope) {
      v.wDeg += inc
    }
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
  def revise(): Traversable[Int]

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
    if (i < 0) {
      a
    } else {
      a(i) = scope(i).dom.size
      sizes(a, i - 1)
    }

  def isBound = scope.forall(_.dom.bound)

  @tailrec
  final def cardSize(p: Int = arity - 1, size: Int = 1): Int =
    if (p < 0) {
      size
    } else {
      val s = scope(p).dom.size
      if (size > Int.MaxValue / s) {
        -1
      } else {
        cardSize(p - 1, size * s)
      }
    }

  final def bigCardSize = scope.foldLeft(BigInt(1))(_ * _.dom.size)

  final def doubleCardSize: Double = {
    var size = 1.0
    var p = arity - 1
    while (p >= 0) {
      size *= scope(p).dom.size
      p -= 1
    }
    size
  }

  final def hasChanged[A](l: Traversable[A], f: A => Boolean) = l.foldLeft(false)(_ | f(_))

  /**
   * A GAC constraint is entailed if it has zero or only one variable with domain size > 1
   */
  def isFree: Boolean = {
    var one = false
    var i = arity - 1
    while (i >= 0) {
      if (scope(i).dom.size > 1) {
        if (one) {
          return false
        }
        one = true
      }
      i -= 1
    }
    true
  }
}
