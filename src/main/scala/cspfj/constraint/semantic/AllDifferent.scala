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
import cspfj.constraint.SimpleRemovals
import scala.collection.immutable.BitSet

final class Node(val n: Int) {
  var list: List[Node] = Nil

  override def toString = n.toString + " : " + list.map(_.n)
}

final class AllDifferent(scope: Variable*) extends AbstractConstraint(null, scope.toArray) with SimpleRemovals with Loggable {

  val offset = scope map { _.dom.allValues.head } min
  val max = scope map { _.dom.allValues.last } max

  def check: Boolean = {
    val union = BitVector.newBitVector(max - offset + 1, false)
    tupleValues.exists { v =>
      if (union.get(v - offset)) return false
      union.set(v - offset)
      true
    }
  }

  class Node(val v: Variable) {
    var d = BitSet.empty ++ (v.values map (_ - offset))

    var child: Node = null
    var next: Node = null
    var rank = 1
    
    if (d.size == 1) filter(d, Set(v))

    override def toString = tree(0)

    def tree(depth: Int): String =
      (0 until depth).map(_ => "--").fold("")(_ + _) + v + " (" + rank + ")\n" +
        (if (child != null) child.tree(depth + 1) else "") +
        (if (next != null) next.tree(depth) else "")

    def flatten: Set[Variable] = {
      var s = Set(v)
      if (child != null) s ++= child.flatten
      if (next != null) s ++= next.flatten
      s
    }
  }

  var root: Node = null

  def add(root: Node, n: Node): Node = {
    if (root == null) n
    else {
      val common = root.d & n.d

      if (root.d == common) {
        n.child = root
        n.rank = root.rank + 1
        if (n.rank == n.d.size) {
          filter(n.d, n.flatten)
        }
        n
      } else if (n.d == common) {
        root.child = add(root.child, n)
        root.rank += 1
        if (root.rank == root.d.size) {
          filter(root.d, root.flatten)
        }
        root
      } else {
        root.next = add(root.next, n)
        root
      }
    }
  }

  var change = false

  def filter(values: Set[Int], except: Set[Variable]) {
    for (v <- scope if !except(v)) {
      for (i <- values.map(v.dom.index) if i >= 0) {
        v.dom.remove(i)
        change = true
      }
    }
  }

  def revise(rh: RevisionHandler, r: Int) = {
    do {
      change = false
      root = scope.map(new Node(_)).reduce(add(_, _))
    } while (change)

    true
  }

  override def toString = "allDifferent" + scope.mkString("(", ", ", ")")

  val getEvaluation = arity.doubleValue * arity
}
