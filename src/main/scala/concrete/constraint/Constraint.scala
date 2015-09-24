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
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.heuristic.Weighted
import concrete.priorityqueues.DLNode
import concrete.priorityqueues.Identified
import concrete.priorityqueues.PTag
import cspom.UNSATException
import scala.reflect.runtime.universe._
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

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

trait StatefulConstraint[State] extends Constraint {
  override def init(ps: ProblemState): Outcome

  override def toString(problemState: ProblemState) = s"${super.toString(problemState)} / ${problemState(this)}"
}

abstract class Constraint(val scope: Array[Variable])
    extends DLNode[Constraint] with Weighted with Identified with PTag with LazyLogging {

  require(scope.nonEmpty)

  if (!scope.distinct.sameElements(scope)) {
    logger.info(s"$this has duplicates in its scope")
  }

  def this(scope: Variable*) = this(scope.toArray)

  private var _id: Int = -1

  def id: Int = _id
  def identify(i: Int): Int = {
    _id = i
    i + 1
  }

  /**
   * arity is the number of variables involved by the constraint
   */
  val arity = scope.size

  /**
   * @return a map containing the positions of variables in the scope of the constraint.
   */
  val position: Map[Variable, Array[Int]] = {
    val pos = new HashMap[Variable, ArrayBuffer[Int]]()

    for ((v, p) <- scope.zipWithIndex) {
      pos.getOrElseUpdate(v, new ArrayBuffer()) += p
    }

    pos.toMap.map { case (k, v) => k -> v.toArray }

  }

  override def equals(o: Any) = o.asInstanceOf[Constraint].id == id

  override def hashCode = id

  @tailrec
  final def controlTuplePresence(problemState: ProblemState, tuple: Array[Int], i: Int = arity - 1): Boolean = {
    /** Need high optimization */

    i < 0 || (problemState.dom(scope(i)).present(tuple(i)) && controlTuplePresence(problemState, tuple, i - 1))

  }

  @tailrec
  final def controlTuplePresence(problemState: ProblemState, tuple: Array[Int], mod: List[Int]): Boolean = {
    /** Need high optimization */

    if (mod eq Nil) {
      assert(controlTuplePresence(problemState, tuple), tuple.mkString("(", ", ", ")") +
        " is not in " + this)
      true
    } else {
      val i = mod.head
      problemState.dom(scope(i)).present(tuple(i)) && controlTuplePresence(problemState, tuple, mod.tail)
    }

  }

  //def inCN = id >= 0

  //  override def weight_=(w: Int) {
  //    //assert(!isEntailed)
  //    assert(inCN)
  //    val inc = w - weight
  //    for (v <- scope) {
  //      v.wDeg += inc
  //    }
  //    super.weight = w
  //  }

  def isConsistent(problemState: ProblemState): Outcome =
    revise(problemState).andThen(_ => problemState)

  def advise(problemState: ProblemState, pos: Int): Int

  def adviseArray(problemState: ProblemState, pos: Array[Int]): Int = {
    var max = -1
    var i = pos.length - 1
    while (i >= 0) {
      max = math.max(max, advise(problemState, pos(i)))
      i -= 1
    }
    max
  }

  def adviseVar(problemState: ProblemState, v: Variable): Int =
    adviseArray(problemState, position(v))

  final def adviseAll(problemState: ProblemState): Int = {
    var max = -1
    var i = arity - 1
    while (i >= 0) {
      max = math.max(max, advise(problemState, i))
      i -= 1
    }
    max
  }

  def init(ps: ProblemState): Outcome // = ps  

  /**
   * The constraint propagator.
   */
  def revise(problemState: ProblemState): Outcome

  def dataSize: Int = ???

  /**
   * @return true iff the constraint is satisfied by the given tuple
   */
  def check(tuple: Array[Int]): Boolean

  def simpleEvaluation: Int

  override def toString = this.getClass.getSimpleName + scope.mkString("(", ", ", ")")

  def toString(problemState: ProblemState) = s"$id: ${this.getClass.getSimpleName}${
    scope.map(v => s"$v ${problemState.dom(v)}").mkString("(", ", ", ")")
  }"

  /**
   * @param variable
   * @return true iff the given variable is involved by the constraint
   */
  final def isInvolved(variable: Variable) = position.contains(variable)

  @tailrec
  final def sizes(problemState: ProblemState, a: Array[Int] = new Array[Int](arity), i: Int = arity - 1): Array[Int] =
    if (i < 0) {
      a
    } else {
      a(i) = problemState.dom(scope(i)).size
      sizes(problemState, a, i - 1)
    }

  def intervalsOnly(problemState: ProblemState): Boolean = {
    var i = arity - 1
    while (i >= 0) {
      if (!problemState.dom(scope(i)).convex) {
        return false
      }
      i -= 1
    }
    true
  }

  @tailrec
  final def cardSize(problemState: ProblemState, p: Int = arity - 1, size: Int = 1): Int =
    if (p < 0) {
      size
    } else {
      val s = problemState.dom(scope(p)).size
      if (size > Int.MaxValue / s) {
        -1
      } else {
        cardSize(problemState, p - 1, size * s)
      }
    }

  final def scopeSize(problemState: ProblemState): Int = {
    var size = 0
    var p = arity - 1
    while (p >= 0) {
      size += problemState.dom(scope(p)).length
      p -= 1
    }
    size
  }

  final def doubleCardSize(problemState: ProblemState): Double = {
    var size = 1.0
    var p = arity - 1
    while (p >= 0) {
      size *= problemState.dom(scope(p)).size
      p -= 1
    }
    size
  }

  /**
   * A GAC constraint is entailed if it has zero or only one variable with domain size > 1
   */
  def isFree(problemState: ProblemState): Boolean = {
    var one = false
    var i = arity - 1
    while (i >= 0) {
      if (problemState.dom(scope(i)).size > 1) {
        if (one) {
          return false
        }
        one = true
      }
      i -= 1
    }
    true
  }

  def controlAssignment(problemState: ProblemState): Boolean = {
    scope.exists(v => problemState.dom(v).size > 1) || check(scope.map(v => problemState.dom(v).head).toArray)

  }

  def controlRevision(ps: ProblemState): Boolean = {
    val adv = adviseAll(ps)
    if (!controlAssignment(ps)) {
      logger.error(s"Assignment of ${toString(ps)} is inconsistent")
      false
    } else {
      revise(ps) match {
        case Contradiction =>
          logger.error(s"${toString(ps)} is not consistent${if (ps.isEntailed(this)) " - entailed" else ""}")
          false
        case finalState: ProblemState =>
          if (!scope.forall(v => ps.dom(v) eq finalState.dom(v))) {
            logger.error(s"${toString(ps)}${if (ps.isEntailed(this)) " - entailed" else ""} was revised (-> ${toString(finalState)})")
            false
          } else if (!(adv < 0 || ps.isEntailed(this) == finalState.isEntailed(this))) {
            logger.error(s"${toString(ps)}: entailment detected")
            false
          } else {
            true
          }
      }
    }

  }

  def entailable: Boolean = true

}
