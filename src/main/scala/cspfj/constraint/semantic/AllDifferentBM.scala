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
import cspfj.problem.Variable
import cspfj.util.BitVector
import scala.annotation.tailrec
import cspfj.constraint.Residues
import cspfj.util.Loggable

final class Node(val n: Int) {
  var list: List[Node] = Nil

  override def toString = n.toString + " : " + list.map(_.n)
}

final class AllDifferentBM(scope: Variable*)
  extends AbstractConstraint(null, scope.toArray)
  with Loggable
  with Residues {

  val offset = scope map { _.dom.allValues.head } min
  val max = scope map { _.dom.allValues.last } max

  def check: Boolean = {
    val union = BitVector.newBitVector(max - offset + 1)
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
  def assign(s: List[Node], m: Set[(Node, Node)]): Set[(Node, Node)] =
    if (s.tail.isEmpty) {
      m
    } else {
      val value = s.head
      val variable = s.tail.head
      //tuple(variable) = scope(variable).dom.index(value)
      fine("matched (" + variable + ", " + value + ")")
      assign(s.tail.tail, m + ((variable, value)))
    }

  def findSupport(pos: Int, idx: Int): Boolean = {
    // Greedy matching
    var matching: Map[Int, Variable] = scope.foldLeft(Map[Int, Variable]())((map, v) =>
      v.values.find(!map.contains(_)) match {
        case None => map
        case Some(value) => map + (value -> v)
      })

    while (true) {
      var preds: Map[Int, List[Variable]] = Map.empty
      var unmatched: List[Int] = Nil

      var pred: Map[Variable, Int] = scope map { _ -> 0 } toMap

      for (i <- matching.values) pred -= i

      var layer = pred.keys.toList

      while (!layer.isEmpty && unmatched.isEmpty) {
        var newLayer: Map[Int, List[Variable]] = Map.empty
        for (u <- layer) {
          for (v <- u.values) {
            if (!preds.contains(v)) {
              newLayer += v -> (u :: newLayer.getOrElse(v, Nil))
            }
          }
        }

        layer = Nil
        for ((v, p) <- newLayer) {
          preds += (v -> p)
          matching.get(v) match {
            case Some(m) => {
              layer ::= m
              pred += m -> v
            }
            case None => unmatched ::= v
          }
        }

        if (unmatched.isEmpty) {
          var unlayered: Set[Int] = Set.empty
          for (u <- scope) {
            for (v <- u.values) {
              if (!preds.contains(v)) {
                  unlayered += v
              }
            }
          }
          println (matching, pred.keys, unlayered)
          return true
        } else {
          
        }
        
        
      }
    }

    true
  }

  override def toString = "allDifferent" + scope.mkString("(", ", ", ")")

  val getEvaluation = arity.doubleValue * arity
}
