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

import scala.annotation.tailrec
import cspfj.constraint.Constraint
import cspfj.util.BitVector
import cspfj.util.UOList
import cspfj.Variable
import cspfj.AdviseCount
import scala.collection.mutable.HashSet

trait AllDiffChecker extends Constraint {

  val offset = scope map { _.dom.firstValue } min
  val max = scope map { _.dom.lastValue } max

  def checkValues(t: Array[Int]): Boolean = {
    val union = BitVector.newBitVector(max - offset + 1)

    t.exists { v =>
      if (union.get(v - offset)) true
      else {
        union.set(v - offset)
        false
      }
    }
  }
}

final class AllDifferent2C(scope: Variable*) extends Constraint(scope.toArray) with AllDiffChecker {

  var q: List[Int] = Nil

  @tailrec
  private def filter(checkedVariable: Int, value: Int, i: Int = arity - 1, mod: UOList[Int] = UOList.empty): UOList[Int] = {

    if (i < 0) mod
    else if (i == checkedVariable) filter(checkedVariable, value, i - 1, mod)
    else {
      val v = scope(i)
      val index = v.dom.index(value)
      if (index >= 0 && v.dom.present(index)) {
        v.dom.remove(index)
        filter(checkedVariable, value, i - 1, mod + i)
      } else filter(checkedVariable, value, i - 1, mod)

    }

  }

  def revise() = {
    var mod = new HashSet[Int]()
    while (q.nonEmpty) {
      val checkedVariable = q.head
      q = q.tail

      val value = scope(checkedVariable).dom.firstValue

      for (i <- filter(checkedVariable, value)) {
        mod += i
        if (scope(i).dom.size == 1) {
          q ::= i
        }
      }
    }
    mod
  }

  var lastAdvise = -1

  def advise(p: Int) = {
    if (lastAdvise != AdviseCount.count) {
      q = Nil
      lastAdvise = AdviseCount.count
    }
    if (scope(p).dom.size > 1) -1 else {
      q ::= p
      arity
    }
  }
  val simpleEvaluation = 3
}
