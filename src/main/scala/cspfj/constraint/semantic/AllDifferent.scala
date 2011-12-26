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

final class Node(val n: Int) {
  var list: List[Node] = Nil

  override def toString = n.toString + " : " + list.map(_.n)
}

final class AllDifferent(scope: Variable*) extends AbstractConstraint(null, scope.toArray) with Loggable {

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

  val s = new Node(-1)

  val t = new Node(-1)

  val varNodes = (0 until arity) map (i => i -> new Node(i)) toMap

  val valNodes = (for (v <- scope; i <- v.values) yield i).distinct map (i => i -> new Node(i)) toMap

  def init(pos: Int, idx: Int) {

    s.list = varNodes.iterator.filter(_._1 != pos).map(_._2).toList

    val value = scope(pos).dom.value(idx)
    tuple(pos) = idx

    fine("Matching for (" + pos + ", " + value + ")")

    for (v <- valNodes.values) v.list = List(t)

    for (p <- 0 until arity if p != pos) {
      varNodes(p).list = Nil
      for (v <- scope(p).values if v != value) {
        varNodes(p).list ::= valNodes(v)
        valNodes(v).list ::= varNodes(p)
      }
    }

  }

  @tailrec
  def assign(s: List[Node], m: Int): Int =
    if (s.tail.isEmpty) {
      m
    } else {
      val value = s.head.n
      val variable = s.tail.head.n
      tuple(variable) = scope(variable).dom.index(value)
      fine("matched (" + variable + ", " + value + ")")
      assign(s.tail.tail, m + 1)
    }

  def matching(pos: Int, idx: Int) = {

    init(pos, idx)

    @tailrec
    def alternate(stack: List[Node], b: Set[Node], matches: Int): Boolean = {
      if (matches >= arity - 1) {
        assert(matches == arity - 1)
        true
      } else if (stack.isEmpty) {
        false
      } else if (stack.head.list.isEmpty) {
        alternate(stack.tail, b, matches)
      } else {
        val first = stack.head.list.head
        stack.head.list = stack.head.list.tail

        if (first == t) {
          alternate(List(s), b, matches + assign(stack, 0))
        } else if (b(first)) {
          alternate(stack, b, matches)
        } else {
          alternate(first :: stack, b + first, matches)
        }
      }
    }

    alternate(List(s), Set.empty, 0)

  }
  
  

  override def toString = "allDifferent" + scope.mkString("(", ", ", ")")

  val getEvaluation = arity.doubleValue * arity
}
