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

final class AllDifferent(scope: Variable*)
  extends AbstractConstraint(null, scope.toArray)
  with Loggable
  with VariableGrainedRemovals {

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

  def seek(h: Hasse[VarInfo]): List[HNode[VarInfo]] = seek(h.roots, BitSet.empty, Nil)

  @tailrec
  def seek(s: List[HNode[VarInfo]], done: BitSet, collected: List[HNode[VarInfo]]): List[HNode[VarInfo]] =
    if (s == Nil) collected
    else {
      val head :: tail = s
      if (done(head.cId)) seek(tail, done, collected)
      else if (head.v.s < head.rank) throw AllDifferent.i
      else if (head.v.s == head.rank)
        seek(stack(head.child, tail), done + head.cId, head :: collected)
      else seek(stack(head.child, tail), done + head.cId, collected)
    }

  /**
   * Stacks n (reversed for efficiency) on s
   */
  @tailrec
  private def stack[A](n: List[A], s: List[A]): List[A] =
    if (n == Nil) s
    else stack(n.tail, n.head :: s)

  private var tree = new Hasse[VarInfo](new VarInclusion)

  override def restoreLvl(l: Int) {
    super.restoreLvl(l)

    val changed = scope filter { v =>
      val vi = info(v)
      if (vi.s == v.dom.size) {
        false
      } else {
        tree.remove(vi)
        true
      }
    }

    changed.foreach { v =>
      val vi = VarInfo(v, dom(v), v.dom.size)
      info += v -> vi
      tree.add(vi)
    }
  }

  def revise(rh: RevisionHandler, rvls: Int): Boolean = revise(rh, modified(rvls).toSeq)

  var info: Map[Variable, VarInfo] = Map.empty

  @tailrec
  private def revise(rh: RevisionHandler, changed: Seq[Variable]): Boolean =
    if (changed.isEmpty) true
    else {

      changed.foreach(v => info.get(v) match {
        case None =>
        case Some(v) => tree.remove(v)
      })

      /**
       * Much more efficient when adding largest domains first (less likely to
       * have supersets)
       */
      changed.sortBy(-_.dom.size).foreach { v =>
        val vi = VarInfo(v, dom(v), v.dom.size)
        info += v -> vi
        tree.add(vi)
      }

      var unsat = false
      val change: Seq[Variable] = try {
        seek(tree).foldLeft(Set[Variable]())((acc, n) =>
          acc ++ filter(rh, n)).toSeq
      } catch {
        case e: Inconsistency => { unsat = true; Seq.empty }
      }

      if (unsat) false
      else revise(rh, change)
    }

  private def filter(rh: RevisionHandler, node: HNode[VarInfo]): Set[Variable] = {
    val vals = values(node.v.d)

    val change: Set[Variable] =
      (scopeSet -- node.stream.map(_.v.v)) filter (remove(_, vals))

    change.foreach(rh.revised(this, _))

    change
  }

  private def remove(v: Variable, vals: List[Int]) = {
    val toRemove = vals.iterator map (v.dom.index) filter (v.dom.present)

    if (toRemove.isEmpty) false
    else {
      toRemove.foreach(v.dom.remove)
      if (v.dom.size == 0) throw AllDifferent.i
      true
    }
  }

  private def values(bv: BitVector) = {
    var l: List[Int] = Nil
    var i = bv.nextSetBit(0)
    while (i >= 0) {
      l ::= i + offset
      i = bv.nextSetBit(i + 1)
    }
    l
  }

  override def toString = "allDifferent" + scope.mkString("(", ", ", ")")

  val getEvaluation = arity.doubleValue * arity
}
