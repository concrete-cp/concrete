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

package concrete.constraint.semantic;

import scala.annotation.tailrec
import concrete.constraint.Constraint
import cspom.util.BitVector
import concrete.Variable
import concrete.constraint.AdviseCount
import scala.collection.mutable.HashSet
import concrete.constraint.AdviseCounts
import concrete.constraint.AdviseCount
import concrete.Domain
import concrete.Contradiction
import concrete.ProblemState
import concrete.Outcome

trait AllDiffChecker extends Constraint {

  val offset = scope.iterator.map(_.initDomain.head).min
  val max = scope.iterator.map(_.initDomain.last).max

  def check(t: Array[Int]): Boolean = {
    //println(t.toSeq)
    var union = collection.mutable.BitSet.empty

    !t.exists { v =>
      union(v - offset) || {
        union += (v - offset)
        false
      }
    }
  }
}

final class AllDifferent2C(scope: Variable*) extends Constraint(scope.toArray) with AllDiffChecker with AdviseCounts {

  var q: List[Variable] = Nil

  def revise(ps: ProblemState): Outcome = {
    // print(this)
    var state = ps
    while (q.nonEmpty) {
      val checkedVariable = q.head
      q = q.tail

      val value = state.dom(checkedVariable).singleValue

      for (v <- scope) {
        if (v != checkedVariable) {
          val od = state.dom(v)
          val nd = od.remove(value)

          if (nd.isEmpty) return Contradiction
          if (od ne nd) {
            state = state.updateDomNonEmpty(v, nd)
            if (nd.size == 1) q ::= v
          }
        }
      }
    }
    state.entailIfFree(this)

  }

  var lastAdvise = -1

  def advise(ps: ProblemState, p: Int) = {

    if (lastAdvise != adviseCount) {
      q = Nil
      lastAdvise = adviseCount
    }
    val v = scope(p)
    if (ps.dom(v).size > 1) {
      -1
    } else {
      q ::= v
      arity
    }
  }
  val simpleEvaluation = 3
}
