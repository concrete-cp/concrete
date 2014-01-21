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

package concrete.constraint.extension;

import scala.annotation.tailrec
import concrete.constraint.Constraint
import concrete.util.Backtrackable
import concrete.util.BitVector
import concrete.util.Loggable
import concrete.Variable
import concrete.UNSATException
import concrete.constraint.Removals
import concrete.Statistic
import concrete.util.SparseSet
import concrete.util.IntSet
import scala.collection.mutable.HashSet

final class ExtensionConstraintSTR3(_scope: Array[Variable], private var table: Array[Array[Int]])
  extends Constraint(_scope) with Removals with Backtrackable[(SparseSet, Array[Array[Int]], Array[IntSet])] {

  private val found = scope map (p => BitVector.newBitVector(p.dom.maxSize))

  override def setLvl(l: Int) {
    super.setLvl(l)
    setLevel(l)
  }

  override def restoreLvl(l: Int) {
    super.restoreLvl(l)
    restoreLevel(l)
  }

  val domSizes = new Array[Int](arity)

  private var init = true

  private var inv = new SparseSet(table.length)

  private val row: Array[Array[Array[Int]]] = scope.map(v => Array.fill(v.dom.size)(Array[Int]()));

  private var cur: Array[Array[Int]] = scope.map(v => new Array[Int](v.dom.size))

  private val dep: Array[Set[(Int, Int)]] = Array.fill(table.length)(Set())

  private var last: Array[IntSet] = null

  def revise(mod: List[Int]) = {
    altering()
    if (init) {
      table = table.filter { t =>
        t.zipWithIndex.forall { case (i, p) => scope(p).dom.present(i) }
      }

      {
        val row = scope.map(v => Array.fill(v.dom.size)(List[Int]()))
        for ((t, r) <- table.zipWithIndex) {
          for ((i, p) <- t.zipWithIndex) {
            row(p)(i) ::= r
          }
        }
        for (p <- 0 until arity; i <- 0 until scope(p).dom.size) {
          this.row(p)(i) = row(p)(i).toArray
        }
      }

      val c = (0 until arity).filter(
        p => scope(p).dom.filter(i => row(i).nonEmpty))
        
      for (p <- 0 until arity; i <- scope(p).indices) {
        cur(p)(i) = row(p)(i).length - 1
        dep(row(p)(i).head) = Set((p, i))
      }
      init = false
      last = scope.map(_.dom.intSet)
      c
    } else {
      var c = new HashSet[Int]()

      for (x <- mod; a <- last(x).iterator if !scope(x).dom.present(a)) {
        val prevMembers = inv.size
        for (k <- 0 to cur(x)(a)) {
          inv += row(x)(a)(k)
        }
        if (prevMembers != inv.size) {
          for (i <- prevMembers until inv.size) {
            val k = inv.dense(i)
            for ((y, b) <- dep(k) if (scope(y).dom.present(b))) {
              var p = cur(y)(b)
              while (p >= 0 && inv(row(y)(b)(p))) {
                p -= 1
              }
              if (p < 0) {
                scope(y).dom.remove(b)
                c += y
              } else {
                if (p != cur(y)(b)) {
                  cur(y)(b) = p
                }
                val t = (y, b)
                dep(k) -= t
                dep(row(y)(b)(p)) += t
              }
            }
          }
        }
      }

      if (c.nonEmpty) {
        last = scope.map(_.dom.intSet)
      }
      c
    }

  }

  def save = {
    //println(this + " <- " + trie.size)
    (inv, cur, last)
  }

  def restore(t: (SparseSet, Array[Array[Int]], Array[IntSet])) {
    inv = t._1
    cur = t._2
    last = t._3
    //println(this + " -> " + trie.size)
  }

  override def checkIndices(t: Array[Int]) = table.exists(_.sameElements(t))

  def checkValues(t: Array[Int]) = throw new UnsupportedOperationException

  private def matches(tuple: Array[Int], base: Array[Int]) = {
    assert(tuple.length == base.length);
    (base, tuple).zipped.forall { (b, t) => b < 0 || b == t }
  }

  def removeTuples(base: Array[Int]) = {
    throw new UnsupportedOperationException
    //    unshareMatrix()
    //    val s = size
    //
    //    //matrixManager.filter(t => !matches(t, base))
    //
    //    size - s;
  }

  def removeTuple(t: Array[Int]) = throw new UnsupportedOperationException

  //def matrixManager = matrixManager

  val prop = (table.length * arity).toDouble / doubleCardSize

  def getEvaluation = (prop * doubleCardSize).toInt

  def simpleEvaluation = math.min(7, scope.count(_.dom.size > 1))

  override def toString = scope.mkString("ExtReduce(", ", ", ")")
  
  override def dataSize = ???
}


