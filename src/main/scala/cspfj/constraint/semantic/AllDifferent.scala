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

package cspfj.constraint.semantic;

import scala.collection.immutable.Queue
import cspfj.constraint.AbstractConstraint
import cspfj.constraint.VariableGrainedRemovals
import cspfj.filter.RevisionHandler
import cspfj.problem.Variable
import cspfj.util.BitVector
import scala.annotation.tailrec

final class AllDifferent(scope: Variable*) extends AbstractConstraint(null, scope.toArray)
  with VariableGrainedRemovals {

  val offset = scope map { _.dom.allValues.head } min
  val max = scope map { _.dom.allValues.last } max

  var queue: Queue[Variable] = Queue.empty

  def check: Boolean = {
    val union = BitVector.newBitVector(max - offset + 1, false)
    tupleValues.exists { v =>
      if (union.get(v - offset)) return false
      union.set(v - offset)
      true
    }
  }

  trait FilterResult

  case class FILT(f: List[Variable]) extends FilterResult
  case object INC extends FilterResult

  private def filter(values: Seq[Int], preserve: Set[Variable], revisator: RevisionHandler): FilterResult = {
    var changed: List[Variable] = List.empty
    for (v <- scope if (!preserve(v)); index <- values map (v.dom.index) if (index >= 0 && v.dom.present(index))) {
      v.dom.remove(index);
      if (v.dom.size < 1) {
        return INC
      }

      revisator.revised(this, v);
      changed ::= v
    }

    FILT(changed)

  }
  //
  //  override def revise(revisator: RevisionHandler, reviseCount: Int): Boolean = {
  //    queue = Queue.empty
  //    varsWithRemovals(reviseCount).map(_._1).filter(_.dom.size == 1).foreach(v => queue = queue.enqueue(v))
  //
  //    while (queue != Nil) {
  //      val (checkedVariable, newQueue) = queue.dequeue
  //      queue = newQueue
  //      val value = checkedVariable.dom.firstValue
  //
  //      if (filter(checkedVariable, value, revisator)) {
  //        return false;
  //      }
  //    }
  //
  //    union.fill(false);
  //    var size = 0;
  //    for (variable <- scope; value <- variable.dom.values) {
  //      if (union.set(value - offset)) {
  //        size += 1
  //        if (size >= arity) return true
  //      }
  //    }
  //
  //    false;
  //  }

  var domains: Map[Seq[Int], Set[Variable]] = Map.empty

  var oldDomains: Map[Variable, Seq[Int]] = Map.empty

  def update(vars: Iterable[Variable]) {
    vars.foreach(v =>
      oldDomains.get(v) match {
        case None =>
        case Some(d) => domains += d -> (domains(d) - v)
      })
  }

  override def revise(revisator: RevisionHandler, reviseCount: Int): Boolean = {

    @tailrec
    def revise(vars: Set[Variable]): Boolean = {
      if (vars.isEmpty) {
        true
      } else {
        val head = vars.head
        val tail = vars.tail

        oldDomains.get(head) match {
          case None =>
          case Some(d) => {
            domains += d -> (domains(d) - head)
          }

        }

        val domain = head.values.toList

        val nbVars = domains.getOrElse(domain, Set.empty) + head

        domains += domain -> nbVars

        if (domain.size == nbVars.size) {
          filter(domain, nbVars, revisator) match {
            case FILT(vars) => {
              update(vars)
              revise(tail ++ vars)
            }
            case INC => false
          }
        } else {
          revise(tail)
        }

      }
    }

    val changed = modified(reviseCount).toSet

    update(changed)

    revise(changed)
  }

  override def toString = "allDifferent" + scope.mkString("(" + ", " + ")")

  val getEvaluation = arity.doubleValue * arity
}
