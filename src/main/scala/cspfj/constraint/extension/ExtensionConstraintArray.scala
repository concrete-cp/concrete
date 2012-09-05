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

package cspfj.constraint.extension;

import cspfj.constraint.Constraint
import cspfj.Variable
import cspfj.util.BitVector
import scala.collection.JavaConversions
import scala.collection.mutable.BitSet
import cspfj.constraint.Removals
import cspfj.constraint.Constraint
import scala.annotation.tailrec
import cspfj.util.Loggable
import cspfj.util.Backtrackable

final class ExtensionConstraintArray(
  scope: Array[Variable],
  private val tuplesArray: Array[Array[Int]])
  extends ExtensionConstraint(scope, new TupleSeq(tuplesArray), false)
  with Removals with Loggable with Backtrackable[Int] {

  private val found =
    (0 until arity) map (p => BitVector.newBitVector(scope(p).dom.maxSize)) toArray

  override def setLvl(l: Int) {
    super.setLvl(l)
    setLevel(l)
  }

  override def restoreLvl(l: Int) {
    super.restoreLvl(l)
    restoreLevel(l)
  }

  private var bound: Int = tuplesArray.length

  def save = bound

  def restore(d: Int) {
    bound = d
  }

  private def remove(i: Int) = {
    altering()
    bound -= 1
    val tmp = tuplesArray(i)
    tuplesArray(i) = tuplesArray(bound)
    tuplesArray(bound) = tmp
  }

  val pos = new MutableList(arity)

  def revise(modified: List[Int]) = {
    //fine("Revising " + this + " : " + mod.toList)
    pos.clear()
    for (p <- 0 until arity) {
      found(p).fill(false)
      pos.add(p)
    }
    println(bound)

    var i = 0
    while (i < bound) {
      if (controlTuplePresence(tuplesArray(i), modified)) {
        setFound(tuplesArray(i), pos)
        i += 1
      } else remove(i)
    }

    val c = filter(pos)

    val card = scope.map(v => BigInt(v.dom.size)).product
    assert(card >= bound, card + " < " + bound + "!")
    if (card == bound) {
      //logger.info("Entailing " + this)
      entail()
    }

    //if (scope.count(_.dom.size > 1) <= 1) entail()

    c

  }

  private def setFound(tuple: Array[Int], pos: MutableList) {
    var i = 0
    while (i < pos.nb) {
      val p = pos(i)
      if (found(p).set(tuple(p)) && found(p).cardinality == scope(p).dom.size) {
        pos.remove(i)
      } else {
        i += 1
      }
    }
  }

  //  @tailrec
  //  private def setFound(tuple: Array[Int], pos: List[Int], rem: List[Int] = Nil): List[Int] = {
  //    if (pos eq Nil) rem
  //    else {
  //      val p = pos.head
  //      if (found(p).set(tuple(p)) && found(p).cardinality == scope(p).dom.size) {
  //        setFound(tuple, pos.tail, rem)
  //      } else
  //        setFound(tuple, pos.tail, p :: rem)
  //    }
  //  }

  private def filter(pos: MutableList): Boolean = {
    var c = false
    for (i <- 0 until pos.nb) {
      val p = pos(i)
      c |= scope(p).dom.filter(i => found(p)(i))
    }
    c
  }

  def tuples = tuplesArray.take(bound)

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

  def getEvaluation = arity * bound

  def simpleEvaluation = math.min(7, scope.count(_.dom.size > 1))

  override def toString = scope.mkString("STR(", ", ", ")")
}

class MutableList(size: Int) {
  private val data = new Array[Int](size)
  private var _nb = 0
  def clear() { _nb = 0 }
  def add(value: Int) {
    data(_nb) = value
    _nb += 1
  }
  def apply(index: Int) = data(index)
  def remove(index: Int) {
    _nb -= 1
    data(index) = data(_nb)
  }
  def nb = _nb
}
