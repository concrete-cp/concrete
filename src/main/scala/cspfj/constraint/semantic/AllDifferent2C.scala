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
import cspfj.constraint.Constraint
import cspfj.problem.Variable
import cspfj.util.BitVector
import scala.annotation.tailrec
import cspfj.constraint.Removals
import cspfj.UNSATException
import cspfj.util.UOList

object AllDifferent2C {
  def filter(scope: Array[Variable], checkedVariable: Int, value: Int): UOList[Int] = {

    def r(i: Int, mod: UOList[Int]): UOList[Int] =
      if (i < 0) mod
      else if (i == checkedVariable) r(i - 1, mod)
      else {
        val v = scope(i)
        val index = v.dom.index(value)
        if (index >= 0 && v.dom.present(index)) {
          v.dom.remove(index)
          r(i - 1, mod + i)
        } else r(i - 1, mod)

      }

    r(scope.size - 1, UOList.empty)

  }
}

final class AllDifferent2C(scope: Variable*) extends Constraint(scope.toArray)
  with Removals {

  val offset = scope map { _.dom.firstValue } min
  val max = scope map { _.dom.lastValue } max
  val union = BitVector.newBitVector(max - offset + 1)

  val scopeA = scope.toArray

  def checkValues(t: Array[Int]): Boolean = {
    union.fill(false)
    t.exists { v =>
      if (union.get(v - offset)) true
      else {
        union.set(v - offset)
        false
      }
    }
  }

  def revise(modified: Seq[Int]) = {

    @tailrec
    def rev(q: UOList[Int], c: Boolean): Boolean =
      if (q.isEmpty) c
      else {
        val checkedVariable = q.head
        val newQueue = q.tail

        val value = scope(checkedVariable).dom.firstValue

        val ch = AllDifferent2C.filter(scopeA, checkedVariable, value)

        rev(newQueue ++ ch.filter(scope(_).dom.size == 1), c || !ch.isEmpty)
      }

    rev(UOList.build(modified.filter(scope(_).dom.size == 1)), false)
    //true
    //if (!checkPigeons) throw UNSATException.e

  }

  def checkPigeons: Boolean = {
    val union = BitVector.newBitVector(max - offset + 1)
    var size = 0;

    def vals(v: Variable, i: Int): Boolean = {
      if (i < 0) false
      else if (union.set(v.dom.value(i) - offset)) {
        size += 1
        if (size >= arity) true
        else vals(v, v.dom.next(i))
      } else vals(v, v.dom.next(i))
    }

    scope.exists(v => vals(v, v.dom.first))
  }
  val getEvaluation = arity
  val simpleEvaluation = 3
}
