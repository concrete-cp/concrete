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
import cspfj.util.UOList
import cspfj.UNSATException

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

final class AllDifferent(scope: Variable*)
  extends AbstractConstraint(null, scope.toArray)
  with Loggable
  with Backtrackable[Set[VarInfo]] {

  private val offset = scope map { _.dom.allValues.head } min
  private val max = scope map { _.dom.allValues.last } max

  def check: Boolean = {
    val union = BitVector.newBitVector(max - offset + 1)
    tupleValues.exists { v =>
      if (union.get(v - offset)) return false
      union.set(v - offset)
      true
    }
  }

  var seeks = 0

  private def seek(h: Hasse[VarInfo]): UOList[HNode[VarInfo]] = {
    seeks += 1
    seek(h.roots, UOList.empty)
  }

  @tailrec
  private def seek(s: UOList[HNode[VarInfo]], collected: UOList[HNode[VarInfo]]): UOList[HNode[VarInfo]] =
    if (s.isEmpty) collected
    else if (s.head.v.sought == seeks)
      seek(s.tail, collected)
    else {
      s.head.v.sought = seeks
      //      if (s.head.removed)
      //        seek(s.head.child ++ s.tail, collected)
      //      else 
      if (s.head.v.s < s.head.rank)
        throw UNSATException.e
      else if (s.head.v.s == s.head.rank)
        seek(s.head.child ++ s.tail, collected + s.head)
      else
        seek(s.head.child ++ s.tail, collected)
    }

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
      val vi = VarInfo(v, v.dom.valueBV(offset), v.dom.size)
      info += v -> vi
      tree.add(vi)
    }

    restoreLevel(l)
  }

  def revise(rvls: Int) {
    @tailrec
    def mod(i: Int, m: List[Variable]): List[Variable] = {
      if (i < 0) m
      else {
        val v = scope(i)
        info.get(v) match {
          case Some(vi) if v.dom.size == vi.s => mod(i - 1, m)
          case _ => mod(i - 1, v :: m)
        }
      }
    }

    revise(mod(arity - 1, Nil))
  }

  var info: Map[Variable, VarInfo] = Map.empty

  @tailrec
  private def revise(changed: List[Variable]) {
    if (!changed.isEmpty) {
      changed.foreach(v => info.get(v) match {
        case None =>
        case Some(v) => tree.remove(v)
      })

      val (singles, other) = changed.partition(_.dom.size == 1)

      val change: List[Variable] =
        if (singles.isEmpty) {

          /**
           * Much more efficient when adding largest domains first (less likely to
           * have supersets)
           */
          changed.sortBy(-_.dom.size).foreach { v =>
            val vi = VarInfo(v, v.dom.valueBV(offset), v.dom.size)
            info += v -> vi
            tree.add(vi)

          }

          seek(tree).flatMap(filter(_)).toList.distinct
        } else {
          (other ++
            singles.flatMap { v =>
              val value = v.dom.firstValue
              for (
                f <- scope if f != v;
                val i = f.dom.index(value);
                if i >= 0 && f.dom.present(i)
              ) yield {
                f.dom.remove(i)
                f
              }
            }).distinct

        }

      if (!change.isEmpty) altering()
      revise(change)
    }
  }

  private var filtered: Set[VarInfo] = Set.empty

  def save = filtered

  def restore(d: Set[VarInfo]) {
    filtered = d
  }

  private def filter(node: HNode[VarInfo]): Set[Variable] =
    if (filtered(node.v)) {
      assert {
        val vals = values(node.v.d)
        (scopeSet -- node.flatten.map(_.v.v)).iterator.filter(remove(_, vals)).isEmpty
      }
      Set.empty

    } else {
      filtered += node.v
      val vals = values(node.v.d)
      (scopeSet -- node.flatten.map(_.v.v)).filter(remove(_, vals))
    }

  private def remove(v: Variable, vals: List[Int]) = {
    val toRemove = vals.iterator map (v.dom.index) filter (i =>
      i >= 0 && v.dom.present(i))

    if (toRemove.isEmpty) false
    else {
      toRemove.foreach(v.dom.remove)
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

  val getEvaluation = arity * arity * (max - offset)
}
