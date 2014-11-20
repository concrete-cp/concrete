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
import concrete.Contradiction
import concrete.UNSATException
import concrete.Variable
import concrete.heuristic.Weighted
import concrete.priorityqueues.DLNode
import concrete.priorityqueues.Identified
import concrete.priorityqueues.PTag
import concrete.ReviseOutcome
import concrete.Domain

object Constraint {

  //  private val cId = new ThreadLocal[Int];
  //  cId.set(0)
  //
  //  def next(): Int = {
  //    val r = cId.get
  //    cId.set(r + 1)
  //    r
  //  }

  val UNARY = 1
  val BINARY = 2
  val TERNARY = 3
  val NP = 7

}

trait Stateless extends Constraint {
  type State = Unit

  def initState: State = Unit

  def revise(domains: IndexedSeq[Domain]): ReviseOutcome[Unit]

  def revise(domains: IndexedSeq[Domain], s: State): ReviseOutcome[State] = revise(domains)

  def isConsistent(domains: IndexedSeq[Domain]): Boolean = revise(domains) != Contradiction

  override final def isConsistent(domains: IndexedSeq[Domain], s: State): Boolean = isConsistent(domains)
}

abstract class Constraint(val scope: Array[Variable])
  extends DLNode[Constraint] with Weighted with Identified with PTag {

  type State <: Any

  def this(scope: Variable*) = this(scope.toArray)

  def initState: State

  var id: Int = -1

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

  override def equals(o: Any) = o.asInstanceOf[Constraint].id == id

  /**
   * @return string description of the constraint
   */
  def getType = getClass.getSimpleName

  override def hashCode = id

  @tailrec
  final def controlTuplePresence(domains: IndexedSeq[Domain], tuple: Array[Int], i: Int = arity - 1): Boolean = {
    /** Need high optimization */

    i < 0 || (domains(i).present(tuple(i)) && controlTuplePresence(domains, tuple, i - 1))

  }

  @tailrec
  final def controlTuplePresence(domains: IndexedSeq[Domain], tuple: Array[Int], mod: List[Int]): Boolean = {
    /** Need high optimization */

    if (mod eq Nil) {
      assert(controlTuplePresence(domains, tuple), tuple.mkString("(", ", ", ")") +
        " is not in " + this)
      true
    } else {
      val i = mod.head
      domains(i).present(tuple(i)) && controlTuplePresence(domains, tuple, mod.tail)
    }

  }

  //  final def isEntailed = entailedAtLevel >= 0

  def inCN = id >= 0

  //  final def disEntail() {
  //    assert(isEntailed)
  //    entailedAtLevel = -1
  //    if (inCN) for (v <- scope) {
  //      v.wDeg += weight
  //    }
  //  }
  //
  //  final def entail() {
  //    if (!isEntailed) {
  //      entailedAtLevel = level
  //      if (inCN) for (v <- scope) {
  //        v.wDeg -= weight
  //      }
  //    }
  //
  //  }

  override def weight_=(w: Int) {
    //assert(!isEntailed)
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

  def isConsistent(domains: IndexedSeq[Domain], state: State): Boolean =
    revise(domains, state) != Contradiction

  def advise(domains: IndexedSeq[Domain], pos: Int): Int

  def advise(domains: IndexedSeq[Domain], v: Variable): Int = advise(domains, position(v))

  final def adviseAll(domains: IndexedSeq[Domain]): Int = {
    var p = arity - 1
    var i = -1
    while (p >= 0) {
      val advOutcome = advise(domains, p)
      i = math.max(i, advOutcome)
      p -= 1
    }
    i
  }

  /**
   * The constraint propagator. Returns true if any domain change was done.
   *
   * @param revisator
   */
  def revise(domains: IndexedSeq[Domain], state: State): ReviseOutcome[State]

  def dataSize: Int = ???

  /**
   * @return true iff the constraint is satisfied by the given tuple
   */
  def check(tuple: Array[Int]): Boolean

  def simpleEvaluation: Int

  override def toString = this.getClass.getSimpleName + scope.mkString("(", ", ", ")")

  /**
   * @param variable
   * @return true iff the given variable is involved by the constraint
   */
  final def isInvolved(variable: Variable) = position.contains(variable)

  @tailrec
  final def sizes(domains: IndexedSeq[Domain], a: Array[Int] = new Array[Int](arity), i: Int = arity - 1): Array[Int] =
    if (i < 0) {
      a
    } else {
      a(i) = domains(i).size
      sizes(domains, a, i - 1)
    }

  def intervalsOnly(domains: IndexedSeq[Domain]): Boolean = {
    var i = arity - 1
    while (i >= 0) {
      if (!domains(i).bound) {
        return false
      }
      i -= 1
    }
    true
  }

  @tailrec
  final def cardSize(domains: IndexedSeq[Domain], p: Int = arity - 1, size: Int = 1): Int =
    if (p < 0) {
      size
    } else {
      val s = domains(p).size
      if (size > Int.MaxValue / s) {
        -1
      } else {
        cardSize(domains, p - 1, size * s)
      }
    }

  final def scopeSize(domains: IndexedSeq[Domain]): Int = {
    var size = 0
    var p = arity - 1
    while (p >= 0) {
      size += domains(p).size
      p -= 1
    }
    size
  }

  final def doubleCardSize(domains: IndexedSeq[Domain]): Double = {
    var size = 1.0
    var p = arity - 1
    while (p >= 0) {
      size *= domains(p).size
      p -= 1
    }
    size
  }

  final def hasChanged[A](l: Traversable[A], f: A => Boolean) = l.foldLeft(false)(_ | f(_))

  /**
   * A GAC constraint is entailed if it has zero or only one variable with domain size > 1
   */
  def isFree(domains: IndexedSeq[Domain]): Boolean = {
    var one = false
    var i = arity - 1
    while (i >= 0) {
      if (domains(i).size > 1) {
        if (one) {
          return false
        }
        one = true
      }
      i -= 1
    }
    true
  }

  def controlAssignment(domains: IndexedSeq[Domain]): Boolean = {
    domains.exists(_.size > 1) || check(domains.map(_.head).toArray)

  }

  def changes(oldDomains: IndexedSeq[Domain], newDomains: IndexedSeq[Domain]): Seq[Int] = {
    var i = arity - 1
    var ch = List[Int]()
    while (i >= 0) {
      if (oldDomains(i) ne newDomains(i)) ch ::= i
      i -= 1
    }
    ch
  }

  def hasChanges(oldDomains: IndexedSeq[Domain], newDomains: IndexedSeq[Domain]): Boolean = {
    var i = arity - 1
    while (i >= 0) {
      if (oldDomains(i) ne newDomains(i)) return true
      i -= 1
    }
    false
  }
}
