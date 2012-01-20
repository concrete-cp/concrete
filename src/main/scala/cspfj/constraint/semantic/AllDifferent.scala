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
import cspfj.util.HNode
import cspfj.util.EnhancedPartialOrdering
import cspfj.util.PredefPO
import cspfj.util.BitVectorInclusion

final case class VarInfo(
  val v: Variable,
  val d: BitVector,
  val s: Int)

final class VarInclusion extends EnhancedPartialOrdering[VarInfo] with PredefPO[VarInfo] {
  val bvi = new BitVectorInclusion
  def lteq(a: VarInfo, b: VarInfo) = bvi.lteq(a.d, b.d)

  override def lt(a: VarInfo, b: VarInfo) = bvi.lt(a.d, b.d)

  def disjoint(a: VarInfo, b: VarInfo) = bvi.disjoint(a.d, b.d)
}

final class Inconsistency extends Exception

object AllDifferent {
  val i = new Inconsistency
}

final class Hierarchy(p: VarInclusion, r: List[HNode[VarInfo]]) extends Hasse[VarInfo](p, r) {
  def seek: List[HNode[VarInfo]] = seek(roots, Set.empty, Nil)

  @tailrec
  def seek(stack: List[HNode[VarInfo]], done: Set[HNode[VarInfo]], collected: List[HNode[VarInfo]]): List[HNode[VarInfo]] =
    if (stack == Nil) collected
    else {
      val head :: tail = stack
      if (done(head)) seek(tail, done, collected)
      else if (head.v.s < head.rank) throw AllDifferent.i
      else if (head.v.s == head.rank) seek(head.child ::: tail, done + head, head :: collected)
      else seek(head.child ::: tail, done + head, collected)
    }

  override def +(v: VarInfo): Hierarchy = new Hierarchy(p, add(v, roots))
}

final class AllDifferent(scope: Variable*)
  extends AbstractConstraint(null, scope.toArray)
  with Loggable
  with VariableGrainedRemovals
  with Backtrackable[Hierarchy] {

  private val offset = scope map { _.dom.allValues.head } min
  private val max = scope map { _.dom.allValues.last } max

  private def dom(v: Variable) = {
    val bv = BitVector.newBitVector(v.dom.lastValue - offset + 1, false)
    v.dom.values.foreach(vl => bv.set(vl - offset))
    bv
  }

  def check: Boolean = {
    val union = BitVector.newBitVector(max - offset + 1, false)
    tupleValues.exists { v =>
      if (union.get(v - offset)) return false
      union.set(v - offset)
      true
    }
  }

  private var tree = new Hierarchy(new VarInclusion, Nil)
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

  def restore(d: Hierarchy) {
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
    //tree = tree.filter(v => !changed(v._1.v))
    tree = scope.foldLeft(tree)((t, v) => t + VarInfo(v, dom(v), v.dom.size))
    try {
      var change: Set[Variable] = Set.empty
      for (node <- tree.seek) {
        val preserve = node.stream.map(_.v.v).toSet
        var changeV = false
        for (v <- scope if !preserve(v)) {
          var i = node.v.d.nextSetBit(0)
          while (i >= 0) {
            val index = v.dom.index(i + offset)
            if (v.dom.present(index)) {
              v.dom.remove(index)
              changeV = true
            }
            i = node.v.d.nextSetBit(i + 1)
          }
          if (changeV) {
            if (v.dom.size == 0) throw AllDifferent.i
            rh.revised(this, v)
            change += v
          }
        }
      }
      if (!change.isEmpty) revise(rh, change)
      else true

    } catch {
      case e: Inconsistency => false
    }
  }

  override def toString = "allDifferent" + scope.mkString("(", ", ", ")")

  val getEvaluation = arity.doubleValue * arity
}
