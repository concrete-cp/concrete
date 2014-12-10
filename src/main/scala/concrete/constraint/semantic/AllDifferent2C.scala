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
import concrete.util.BitVector
import concrete.Variable
import concrete.constraint.AdviseCount
import scala.collection.mutable.HashSet
import concrete.constraint.AdviseCounts
import concrete.constraint.AdviseCount

import concrete.Revised
import concrete.Domain
import concrete.Contradiction
import concrete.ReviseOutcome

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

  type State = Unit
  def initState = Unit

  var q: List[Int] = Nil

  @tailrec
  private def filter(domains: IndexedSeq[Domain], checkedVariable: Int, value: Int, i: Int = arity - 1, mod: List[Domain] = Nil): List[Domain] = {

    if (i < 0) {
      mod
    } else if (i != checkedVariable && domains(i).present(value)) {
      filter(domains, checkedVariable, value, i - 1, domains(i).remove(value) :: mod)
    } else {
      filter(domains, checkedVariable, value, i - 1, domains(i) :: mod)
    }

  }

  def revise(domains: IndexedSeq[Domain], s: State): ReviseOutcome[Unit] = {
    // print(this)
    var mod = domains.toArray.clone
    while (q.nonEmpty) {
      val checkedVariable = q.head
      q = q.tail

      val value = mod(checkedVariable).head

      var p = arity - 1
      while (p >= 0) {
        if (p != checkedVariable) {
          val nd = mod(p).remove(value)
          if (nd.isEmpty) return Contradiction
          if (mod(p) ne nd) {
            mod(p) = nd
            if (nd.size == 1) q ::= p
          }
        }
        p -= 1
      }

    }
    // println(s" -> $this")
    Revised(mod, isFree(mod))
  }

  var lastAdvise = -1

  def advise(domains: IndexedSeq[Domain], p: Int) = {

    if (lastAdvise != adviseCount) {
      q = Nil
      lastAdvise = adviseCount
    }
    if (domains(p).size > 1) {
      -1
    } else {
      q ::= p
      arity
    }
  }
  val simpleEvaluation = 3
}
