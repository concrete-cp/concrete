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
import cspfj.filter.RevisionHandler
import cspfj.problem.Variable
import cspfj.util.BitVector

final class AllDifferent(scope: Variable*) extends AbstractConstraint(null, scope.toArray)
  with VariableGrainedRemovals {

  val offset = scope map { _.dom.allValues.min } min
  val max = scope map { _.dom.allValues.max } max
  val union = BitVector.newBitVector(max - offset + 1, false)

  var queue: Queue[Variable] = Queue.empty

  def check: Boolean = {
    union.fill(false)
    tupleValues.exists { v =>
      if (union.get(v - offset)) return false
      union.set(v - offset)
      true
    }
  }

  private def filter(checkedVariable: Variable, value: Int, revisator: RevisionHandler): Boolean = {
    for (v <- scope if v != checkedVariable) {
      val index = v.dom.index(value)
      if (index >= 0 && v.dom.present(index)) {
        v.dom.remove(index);
        if (v.dom.size < 1) {
          return true;
        } else if (v.dom.size == 1) {
          queue = queue.enqueue(v)
        }
        revisator.revised(this, v);
      }

    }
    false;
  }

  override def revise(revisator: RevisionHandler, reviseCount: Int): Boolean = {
    queue = Queue.empty.enqueue(varsWithRemovals(reviseCount).filter(_.dom.size == 1).toList)

    while (queue != Nil) {
      val (checkedVariable, newQueue) = queue.dequeue
      queue = newQueue
      val value = checkedVariable.dom.firstValue

      if (filter(checkedVariable, value, revisator)) {
        return false;
      }
    }

    union.fill(false);
    var size = 0;
    for (v <- scope; i <- v.dom.values) {
      if (union.set(v.dom.value(i) - offset)) {
        size += 1
        if (size >= arity) return true
      }
    }

    false;
  }

  override def toString = "allDifferent" + scope.iterator

  val getEvaluation = arity.doubleValue * arity
}
