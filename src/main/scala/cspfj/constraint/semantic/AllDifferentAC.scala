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
import cspfj.problem.Variable
import cspfj.util.BitVector
import scala.annotation.tailrec
import cspfj.constraint.Removals

final class AllDifferentAC(scope: Variable*) extends AbstractConstraint(null, scope.toArray)
  with Removals {

  val offset = scope map { _.dom.allValues.min } min
  val max = scope map { _.dom.allValues.max } max
  val union = BitVector.newBitVector(max - offset + 1)

  def check: Boolean = {
    union.fill(false)
    tupleValues.exists { v =>
      if (union.get(v - offset)) return false
      union.set(v - offset)
      true
    }
  }

  private def filter(checkedVariable: Variable, value: Int): List[Variable] = {
    var mod: List[Variable] = Nil
    for (v <- scope if v != checkedVariable) {
      val index = v.dom.index(value)
      if (index >= 0 && v.dom.present(index)) {
        v.dom.remove(index);
        if (v.dom.size < 1) {
          throw AllDifferent.i
        } else if (v.dom.size == 1) {
          mod ::= v
        }
      }
    }
    mod;
  }

  def revise(modified: Seq[Int]): Boolean = {

    @tailrec
    def rev(q: Queue[Variable]) {
      if (!q.isEmpty) {
        val (checkedVariable, newQueue) = q.dequeue

        val value = checkedVariable.dom.firstValue

        rev(filter(checkedVariable, value).foldLeft(newQueue)(_.enqueue(_)))
      }
    }

    try {
      rev(modified.map(scope).filter(_.dom.size == 1).foldLeft(Queue[Variable]())(_.enqueue(_)))
      //true
      checkPigeons

    } catch {
      case e: Inconsistency => false
    }

  }

  def checkPigeons: Boolean = {
    union.fill(false);
    var size = 0;
    for (variable <- scope; value <- variable.dom.values) {
      if (union.set(value - offset)) {
        size += 1
        if (size >= arity) return true
      }
    }

    false;
  }

  override def toString = "allDifferent" + scope.iterator

  val getEvaluation = arity.doubleValue * arity
}
