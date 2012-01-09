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
import cspfj.util.UniversalSet
import cspfj.util.SetInclusion

final class AllDifferent(scope: Variable*)
  extends AbstractConstraint(null, scope.toArray)
  with Loggable
  with VariableGrainedRemovals
  with Backtrackable[Hasse[(Variable, BitSet)]] {

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

  private var tree: Hasse[(Variable, Set[Int])] = Hasse.empty(
    new PartialOrdering[(Variable, Set[Int])] {
      val si = new SetInclusion[Int]
      override def lteq(a: (Variable, Set[Int]), b: (Variable, Set[Int])) = si.lteq(a._2, b._2)
    }, (null, UniversalSet), (null, Set.empty)) //scope.foldLeft(List[BDom]())((t, v) => add(v, dom(v), t)) //Nil

  def save() = tree

  def restore(d: List[BDom[Variable]]) {
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

  private def add(v: Variable, dom: BitSet, tree: List[BDom[Variable]]): List[BDom[Variable]] = {
    var subset = false

    val nt = tree map { p =>
      if (dom.subsetOf(p.dom)) {
        subset = true
        val child = add(v, dom, p.child)
        BDom(v, dom, child, 1 + child.map(_.rank).sum)
      } else if (p.dom.subsetOf(dom)) {
        subset = true
        BDom(v, dom, List(p), p.rank + 1)
      } else p
    }

    if (subset) nt else BDom(v, dom, Nil, 1) :: nt
    //    if (tree == Nil) BDom(v, dom, Nil, 1) :: Nil
    //    else if (tree.head.dom.subsetOf(dom)) {
    //
    //      val (subsets, others) = tree.tail.partition(bd => bd.dom.subsetOf(dom))
    //      BDom(v, dom, tree.head :: subsets, 1 + tree.head.rank) :: others
    //
    //    } else if (dom.subsetOf(tree.head.dom)) {
    //
    //      BDom(tree.head.v, tree.head.dom, add(v, dom, tree.head.child), 1 + tree.head.rank) :: tree.tail
    //
    //    } else tree.head :: add(v, dom, tree.tail)

  }

  private def clean(tree: List[BDom], variables: Set[Variable]): (List[BDom], Set[Variable]) = {
    if (tree == Nil) (Nil, variables)
    else if (variables.isEmpty) (tree, variables)
    else if (variables.contains(tree.head.v)) {

      val (child, remaining) = clean(tree.head.child, variables - tree.head.v)
      val (tail, remaining2) = clean(tree.tail, remaining)
      (child ::: tail, remaining2)

    } else {

      val (child, remaining) = clean(tree.head.child, variables)
      val rank = 1 + child.map(_.rank).sum
      val (tail, remaining2) = clean(tree.tail, remaining)
      (BDom(tree.head.v, tree.head.dom, child, rank) :: tail, remaining2)

    }
  }

  def toString(tree: List[BDom], depth: Int): String = {
    if (tree == Nil) ""
    else (0 until depth).map(_ => "--").reduce(_ + _) + tree.head.v + " (" + tree.head.rank + ")\n" +
      toString(tree.head.child, depth + 1) +
      toString(tree.tail, depth)

  }

  @tailrec
  private def filter(tree: List[BDom], stack: List[BDom], modified: Set[Variable]): Set[Variable] = {
    if (tree == Nil) {

      if (stack == Nil) modified
      else filter(stack, Nil, modified)

    } else if (tree.head.rank >= tree.head.size) {

      val preserve = flatten(tree.head.child) + tree.head.v
      val changed = performFilter(preserve, tree.head.dom.toList)

      changed.headOption match {
        case Some(first) if first.dom.size == 0 => changed
        case _ => filter(tree.head.child, stack ++ tree.tail, modified ++ changed)
      }

    } else filter(tree.head.child, stack ++ tree.tail, modified)

  }

  @tailrec
  private def performFilter(v: Variable, values: List[Int], change: Boolean): Boolean = {
    if (values == Nil) change
    else {
      val i = v.dom.index(values.head + offset)
      if (i >= 0 && v.dom.present(i)) {
        v.dom.remove(i)
        if (v.dom.size == 0) true
        else performFilter(v, values.tail, true)
      } else performFilter(v, values.tail, change)
    }
  }

  private def performFilter(preserve: Set[Variable], values: List[Int]): Set[Variable] = {
    var changed: Set[Variable] = Set.empty
    for (v <- scope) {
      if (!preserve.contains(v) && performFilter(v, values, false)) {
        if (v.dom.size == 0) return Set(v)
        changed += v
      }
    }
    changed
  }

  private def flatten(tree: List[BDom]): Set[Variable] = {
    if (tree == Nil) Set.empty
    else flatten(tree.head.child) ++ flatten(tree.tail) + tree.head.v
  }

  def revise(rh: RevisionHandler, rvls: Int): Boolean = revise(rh, modified(rvls).toSet)

  private def revise(rh: RevisionHandler, changed: Set[Variable]): Boolean = {
    altering()
    tree = clean(tree, changed)._1
    tree = changed.foldLeft(tree)((t, v) => add(v, dom(v), t))
    val vars = filter(tree, Nil, Set.empty)

    if (vars.isEmpty) true
    else if (vars.exists(_.dom.size == 0)) false
    else {
      vars.foreach(rh.revised(this, _))
      revise(rh, vars)
    }
  }

  override def toString = "allDifferent" + scope.mkString("(", ", ", ")")

  val getEvaluation = arity.doubleValue * arity
}
