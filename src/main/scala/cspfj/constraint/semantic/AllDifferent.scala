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
  val s: Int) {
  var sought = -1
}

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
  with Backtrackable[Set[VarInfo]] {

  private val offset = scope map { _.dom.allValues.head } min
  private val max = scope map { _.dom.allValues.last } max

  private def dom(v: Variable) = {
    val bv = BitVector.newBitVector(v.dom.lastValue - offset + 1)
    v.dom.values.foreach(vl => bv.set(vl - offset))
    bv
  }

  def check: Boolean = {
    val union = BitVector.newBitVector(max - offset + 1)
    tupleValues.exists { v =>
      if (union.get(v - offset)) return false
      union.set(v - offset)
      true
    }
  }

  var seeks = 0

  def seek(h: Hasse[VarInfo]): List[HNode[VarInfo]] = {
    seeks += 1
    seek(h.roots, Nil)
  }

  @tailrec
  def seek(s: List[HNode[VarInfo]], collected: List[HNode[VarInfo]]): List[HNode[VarInfo]] =
    if (s == Nil) collected
    else {
      val head :: tail = s
      if (head.v.sought == seeks) seek(tail, collected)
      else if (head.v.s < head.rank) throw AllDifferent.i
      else if (head.v.s == head.rank) {
        head.v.sought = seeks
        seek(stack(head.child, tail), head :: collected)
      } else {
        head.v.sought = seeks
        seek(stack(head.child, tail), collected)
      }
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

    restoreLevel(l)
  }

  def revise(rvls: Int): Boolean = {
    //val mod = modified(rvls)
    //    assert(mod.forall(v => info.get(v) match {
    //      case Some(vi) => v.dom.size != vi.s
    //      case None => true
    //    }))
    //assert((scopeSet -- mod).forall(v => v.dom.size == info(v).s))
    val mod = scope filter (v => info.get(v) match {
      case Some(vi) => v.dom.size != vi.s
      case None => true
    })
    revise(mod)
  }

  var info: Map[Variable, VarInfo] = Map.empty

  @tailrec
  private def revise(changed: Seq[Variable]): Boolean =
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
      val change: Seq[Variable] = try seek(tree).flatMap(filter).distinct
      catch {
        case e: Inconsistency => { unsat = true; Seq.empty }
      }

      if (unsat) false
      else revise(change)
    }

  private var filtered: Set[VarInfo] = Set.empty

  def save = filtered

  def restore(d: Set[VarInfo]) {
    filtered = d
  }

  private def filter(node: HNode[VarInfo]) =
    if (filtered(node.v)) {
      assert {
        val vals = values(node.v.d)
        (scopeSet -- node.flatten.map(_.v.v)).iterator.filter(remove(_, vals)).isEmpty
      }
      Set.empty

    } else {
      altering()
      filtered += node.v
      val vals = values(node.v.d)
      (scopeSet -- node.flatten.map(_.v.v)).iterator.filter(remove(_, vals))
    }

  private def remove(v: Variable, vals: List[Int]) = {
    val toRemove = vals.iterator map (v.dom.index) filter (i =>
      i >= 0 && v.dom.present(i))

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

  override def setLvl(l: Int) {
    super.setLvl(l)
    setLevel(l)
  }

  override def toString = "allDifferent" + scope.mkString("(", ", ", ")")

  val getEvaluation = arity.doubleValue * arity * (max - offset)
}
