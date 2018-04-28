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

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete.heuristic.Weighted
import concrete.priorityqueues.{DLNode, Identified, PTag}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import java.util

object Constraint {
  val UNARY = 1
  val BINARY = 2
  val TERNARY = 3
  val NP = 7
}

trait StatefulConstraint[+State <: AnyRef] extends Constraint {
  override def init(ps: ProblemState): Outcome

  override def toString(problemState: ProblemState) = s"${super.toString(problemState)} / ${problemState(this)}"

  def data(ps: ProblemState) = ps(this)

  def updateState[S >: State <: AnyRef](ps: ProblemState, value: S): ProblemState = ps.updateState(this, value)
}

abstract class Constraint(val scope: Array[Variable])
  extends DLNode[Constraint] with Weighted with Identified with PTag with LazyLogging with AdviseCounts {

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

  private var modified: util.BitSet = _

  private var timestamp = -1

  private var _id: Int = -1

  override def register(ac: AdviseCount): this.type = {
    modified = new util.BitSet(arity)
    super.register(ac)
  }

  def revise(problemState: ProblemState, modified: BitVector): Outcome

  final def consistent(ps: ProblemState): Outcome = {
    consistent(ps, BitVector(modified))
  }

  def consistent(problemState: ProblemState, modified: Traversable[Int]): Outcome = {
    revise(problemState) match {
      case _: ProblemState => problemState
      case c: Contradiction => c
    }
  }

  final def revise(problemState: ProblemState): Outcome = {
    val modResult = clearMod()
    if (modResult.isEmpty) {
      logger.info(s"${this.toString(problemState)}: asked revision but modified is empty")
      problemState
    } else {
      revise(problemState, modResult).dueTo((this, modVars(modResult)))
    }
  }

  def clearMod(): BitVector = {
    val result = BitVector(modified)
    modified.clear() //= BitVector.empty // Arrays.fill(removals, -1)
    result
  }

  /**
    * arity is the number of variables involved by the constraint
    */
  def arity: Int = scope.length

  protected def modVars(modified: BitVector): Traversable[Variable] = modified.view.map(scope)

  def toString(problemState: ProblemState) = s"${this.getClass.getSimpleName}${
    scope.map(v => s"$v ${problemState.dom(v)}").mkString("(", ", ", ")")
  }"

//  if (logger.underlying.isWarnEnabled && !scope.distinct.sameElements(scope)) {
//    logger.warn(s"$this has duplicates in its scope")
//  }

  def skip(modified: BitVector): Int = {
    val head = modified.nextSetBit(0)
    if (head < 0) {
      -1
    } else if (modified.nextSetBit(head + 1) < 0) {
      head
    } else {
      -1
    }
  }

  def this(scope: Variable*) = this(scope.toArray)

  def identify(i: Int): Int = {
    _id = i
    i + 1
  }

  override def equals(o: Any): Boolean = o.asInstanceOf[Constraint].id == id

  def id: Int = _id

  override def hashCode: Int = id

  @tailrec
  final def ctp(doms: Array[Domain], tuple: Array[Int], i: Int = arity - 1): Boolean = {
    /* Need high optimization */
    i < 0 || (doms(i).contains(tuple(i)) && ctp(doms, tuple, i - 1))
  }

  /**
    * Same event for all positions
    * Mostly used when a variable is several times in the scope
    */
  def eventArray(problemState: ProblemState, event: Event, positions: Array[Int]): Int = {
    var max = Int.MinValue
    var i = positions.length - 1
    while (i >= 0) {
      max = math.max(max, this.event(problemState, event, positions(i)))
      i -= 1
    }
    max
  }

  def event(problemState: ProblemState, event: Event, pos: Int, alwaysMod: Boolean = false): Int = {
    assert(adviseCount >= 0)

    val eval = advise(problemState, event, pos)

    if (eval >= 0 || alwaysMod) {
      //println(s"advising $this")
      if (timestamp != adviseCount) {
        clearMod()
        timestamp = adviseCount
      }

      modified.set(pos)
    }
    eval
  }

  def init(ps: ProblemState): Outcome // = ps

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
      val adv = eventAll(ps, alwaysMod = true)
      adv == -2 || (revise(ps) match {
        case Contradiction(cause, from, to) =>
          logger.error(s"${toString(ps)} is not consistent, $cause ($from) lead to $to")
          false
        case finalState: ProblemState =>
          if (!scope.forall(v => ps.dom(v) eq finalState.dom(v))) {
            logger.error(s"${toString(ps)}} was revised (-> ${
              diff(ps, finalState).map { case (v, (_, a)) => s"$v <- $a" }.mkString(", ")
            })")
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

  final def eventAll(problemState: ProblemState, event: Event = Assignment, alwaysMod: Boolean = false): Int = {
    var max = Int.MinValue
    var i = arity - 1
    while (i >= 0) {
      max = math.max(max, this.event(problemState, event, i, alwaysMod))
      i -= 1
    }
    max
  }

  def controlAssignment(problemState: ProblemState): Boolean = {
    !scope.forall(problemState.dom(_).isAssigned) || check(scope.map(problemState.dom(_).singleValue))
  }

  override def incrementWeight(): Unit = {
    super.incrementWeight()
    for (v <- scope) v.incrementWeight()
  }

  def diff(ps1: ProblemState, ps2: ProblemState): Seq[(Variable, (Domain, Domain))] = {
    for (v <- scope if ps1.dom(v) ne ps2.dom(v)) yield {
      v -> ((ps1.dom(v), ps2.dom(v)))
    }
  }

  protected def advise(problemState: ProblemState, event: Event, pos: Int): Int

}
