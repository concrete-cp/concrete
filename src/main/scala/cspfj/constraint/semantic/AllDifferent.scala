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

object Inconsistency extends Exception

final class Hierarchy(po: VarInclusion, roots: List[HNode[VarInfo]]) extends Hasse[VarInfo](po, roots) {
  def seek: Option[HNode[VarInfo]] = seek(roots, Set.empty)
  @tailrec
  def seek(stack: List[HNode[VarInfo]], done: Set[HNode[VarInfo]]): Option[HNode[VarInfo]] =
    if (stack == Nil) None
    else {
      val head :: tail = stack
      if (done(head)) seek(tail, done)
      else if (head.v.s > head.rank) throw Inconsistency
      else if (head.v.s == head.rank) Some(head)
      else seek(head.child ::: tail, done + head)
    }

  override def +(v: VarInfo): Hierarchy = new Hierarchy(po, add(v, roots))
}

final class AllDifferent(scope: Variable*)
  extends AbstractConstraint(null, scope.toArray)
  with Loggable
  with VariableGrainedRemovals
  with Backtrackable[Hierarchy] {

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
    tree = changed.foldLeft(tree)((t, v) => t + VarInfo(v, v.dom.getBitVector, v.dom.size))
    try {
      tree.seek match {
        case None => true
        case Some(node) => {
          val preserve = node.stream.map(_._1.v).toSet
          for (v <- scope if !preserve(v)) {}
          true
        }
      }
    } catch {
      case Inconsistency => false
    }
  }

  override def toString = "allDifferent" + scope.mkString("(", ", ", ")")

  val getEvaluation = arity.doubleValue * arity
}
