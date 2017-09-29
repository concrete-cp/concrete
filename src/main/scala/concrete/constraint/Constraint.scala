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
package constraint

import com.typesafe.scalalogging.LazyLogging
import concrete.heuristic.Weighted
import concrete.priorityqueues.{DLNode, Identified, PTag}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

trait StatefulConstraint[State <: AnyRef] extends Constraint {
  override def init(ps: ProblemState): Outcome

  override def toString(problemState: ProblemState) = s"${super.toString(problemState)} / ${problemState(this)}"

  def data(ps: ProblemState) = ps(this)

  def updateState(ps: ProblemState, value: State): ProblemState = ps.updateState(this, value)
}

abstract class Constraint(val scope: Array[Variable])
  extends DLNode[Constraint] with Weighted with Identified with PTag with LazyLogging { //with Removals {

  require(scope.nonEmpty)

  /**
    * @return a map containing the positions of variables in the scope of the constraint.
    */
  val position: Map[Variable, Array[Int]] = {
    val pos = new mutable.HashMap[Variable, ArrayBuffer[Int]]()

    for ((v, p) <- scope.zipWithIndex) {
      pos.getOrElseUpdate(v, new ArrayBuffer()) += p
    }

    pos.mapValues(_.toArray).toMap //case (k, v) => k -> v.toArray }

  }

  val positionInVariable: Array[Int] = Array.fill(arity)(-1)
  private var _id: Int = -1

  if (logger.underlying.isWarnEnabled & !scope.distinct.sameElements(scope)) {
    logger.warn(s"$this has duplicates in its scope")
  }

  def this(scope: Variable*) = this(scope.toArray)

  def identify(i: Int): Int = {
    _id = i
    i + 1
  }

  override def equals(o: Any): Boolean = o.asInstanceOf[Constraint].id == id

  override def hashCode: Int = id

  def id: Int = _id

  @tailrec
  final def controlTuplePresence(problemState: ProblemState, tuple: Array[Int], i: Int = arity - 1): Boolean = {
    /* Need high optimization */

    i < 0 || (problemState.dom(scope(i)).present(tuple(i)) && controlTuplePresence(problemState, tuple, i - 1))

  }

  @tailrec
  final def controlTuplePresence(problemState: ProblemState, tuple: Array[Int], mod: List[Int]): Boolean = {
    /* Need high optimization */

    if (mod eq Nil) {
      assert(controlTuplePresence(problemState, tuple), tuple.mkString("(", ", ", ")") +
        " is not in " + this)
      true
    } else {
      val i = mod.head
      problemState.dom(scope(i)).present(tuple(i)) && controlTuplePresence(problemState, tuple, mod.tail)
    }

  }

  def consistent(problemState: ProblemState): Outcome = {
    revise(problemState) match {
      case _: ProblemState => problemState
      case c: Contradiction => c
    }
  }

  def advise(problemState: ProblemState, event: Event, position: Int): Int

  /**
    * Same event for all positions
    * Mostly used when a variable is several times in the scope
    */
  def adviseArray(problemState: ProblemState, event: Event, positions: Array[Int]): Int = {
    var max = -1
    var i = positions.length - 1
    while (i >= 0) {
      max = math.max(max, advise(problemState, event, positions(i)))
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

  override def toString: String = this.getClass.getSimpleName + scope.mkString("(", ", ", ")")

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
      val s = problemState.card(scope(p))
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
      size += problemState.card(scope(p))
      p -= 1
    }
    size
  }

  /**
    * arity is the number of variables involved by the constraint
    */
  def arity: Int = scope.length

  final def doubleCardSize(problemState: ProblemState): Double = {
    var size = 1.0
    var p = arity - 1
    while (p >= 0) {
      size *= problemState.card(scope(p))
      p -= 1
    }
    size
  }

  def singleFree(ps: ProblemState): Option[Int] = {
    var f = -1
    var i = arity - 1
    while (i >= 0) {
      if (!ps.dom(scope(i)).isAssigned) {
        if (f >= 0) return None
        f = i
      }
      i -= 1
    }
    if (f < 0) None else Some(f)
  }

  def controlRevision(ps: ProblemState): Boolean = {

    if (!controlAssignment(ps)) {
      logger.error(s"Assignment of ${toString(ps)} is inconsistent")
      false
    } else {
      val adv = adviseAll(ps)
      adv == -2 || (revise(ps) match {
        case Contradiction(cause, from, to) =>
          logger.error(s"${toString(ps)} is not consistent, $cause ($from) lead to $to")
          false
        case finalState: ProblemState =>
          if (!scope.forall(v => ps.dom(v) eq finalState.dom(v))) {
            logger.error(s"${toString(ps)}} was revised (-> ${toString(finalState)})")
            false
          } else if (!(adv < 0 || ps.entailed.hasInactiveVar(this) == finalState.entailed.hasInactiveVar(this))) {
            logger.error(s"${toString(ps)}: entailment detected")
            false
          } else {
            true
          }
      })

    }
  }

  final def adviseAll(problemState: ProblemState): Int = {
    var max = Int.MinValue
    var i = arity - 1
    while (i >= 0) {
      max = math.max(max, advise(problemState, Assignment, i))
      i -= 1
    }
    max
  }

  def toString(problemState: ProblemState) = s"${this.getClass.getSimpleName}${
    scope.map(v => s"$v ${problemState.dom(v)}").mkString("(", ", ", ")")
  }"

  def controlAssignment(problemState: ProblemState): Boolean = {
    !scope.forall(problemState.dom(_).isAssigned) || check(scope.map(problemState.dom(_).singleValue))
  }

  override def incrementWeight(): Unit = {
    super.incrementWeight()
    for (v <- scope) v.incrementWeight()
  }

  //def entailable: Boolean = true

}
