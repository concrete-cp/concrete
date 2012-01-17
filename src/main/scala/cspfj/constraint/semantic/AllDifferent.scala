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
import cspfj.constraint.Residues
import cspfj.util.Loggable
import scala.collection.immutable.BitSet
import cspfj.util.Backtrackable
import scala.util.Random
import cspfj.util.Hasse
import cspfj.util.SetInclusion

final class AllDifferent(scope: Variable*)
  extends AbstractConstraint(null, scope.toArray)
  with Loggable
  with VariableGrainedRemovals
  with Backtrackable[Hasse[(Variable, Set[Int])]] {

  private val offset = scope map { _.dom.allValues.head } min
  private val max = scope map { _.dom.allValues.last } max

  private def dom(v: Variable) = BitSet.empty ++ (v.dom.values map { _ - offset })

  def check: Boolean = {
    val union = BitVector.newBitVector(max - offset + 1, false)
    tupleValues.exists { v =>
      if (union.get(v - offset)) return false
      union.set(v - offset)
      true
    }
  }

  private var tree: Hasse[(Variable, Set[Int])] = null
//  Hasse.empty(
//    new PartialOrdering[(Variable, Set[Int])] {
//      val si = new SetInclusion[Int]
//      def tryCompare(a: (Variable, Set[Int]), b: (Variable, Set[Int])) =
//        if (lteq(a, b)) {
//          if (lteq(b, a)) Some(0)
//          else Some(1)
//        } else None
//      override def lteq(a: (Variable, Set[Int]), b: (Variable, Set[Int])) = si.lteq(a._2, b._2)
//    }) //scope.foldLeft(List[BDom]())((t, v) => add(v, dom(v), t)) //Nil

  def save() = tree

  def restore(d: Hasse[(Variable, Set[Int])]) {
    tree = d
  }

  override def setLvl(l: Int) {
    super.setLvl(l)
    setLevel(l)
  }

  override def restoreLvl(l: Int) {
    super.restoreLvl(l)
    restoreLevel(l)
  }

  def revise(rh: RevisionHandler, rvls: Int): Boolean = revise(rh, modified(rvls).toSet)

  private def revise(rh: RevisionHandler, changed: Set[Variable]): Boolean = {
    altering()
    true
    //    tree = clean(tree, changed)._1
    //    tree = changed.foldLeft(tree)((t, v) => add(v, dom(v), t))
    //    val vars = filter(tree, Nil, Set.empty)
    //
    //    if (vars.isEmpty) true
    //    else if (vars.exists(_.dom.size == 0)) false
    //    else {
    //      vars.foreach(rh.revised(this, _))
    //      revise(rh, vars)
    //    }
  }

  override def toString = "allDifferent" + scope.mkString("(", ", ", ")")

  val getEvaluation = arity.doubleValue * arity
}
