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
import cspfj.problem.Variable
import cspfj.util.BitVector
import scala.collection.JavaConversions
import scala.collection.mutable.BitSet
import cspfj.constraint.Removals
import cspfj.constraint.Constraint
import scala.annotation.tailrec
import cspfj.util.Loggable
import cspfj.util.Backtrackable

final class ExtensionConstraintDynamic(
  scope: Array[Variable],
  private var tupleSet: TupleSet,
  shared: Boolean) extends ExtensionConstraint(scope, tupleSet, shared)
  with Removals with Loggable with Backtrackable[(List[Array[Int]], Int)] {

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

  def revise(modified: Seq[Int]) = {
    //fine("Revising " + this + " : " + mod.toList)
    found.foreach(_.fill(false))

    val mod = modified.toList

    filterTuples { tuple =>
      if (controlTuplePresence(tuple, mod)) {
        setFound(tuple, found)
        true
      } else false
    }

    val c = filter(found)

    val card = scope.map(v => BigInt(v.dom.size)).product
    assert(card >= size, card + " < " + size + "!")
    if (card == size) {
      //logger.info("Entailing " + this)
      entail()
    }

    c

  }

  @tailrec
  private def setFound(tuple: Array[Int], found: Array[BitVector], p: Int = arity - 1) {
    if (p >= 0) {
      found(p).set(tuple(p))
      setFound(tuple, found, p - 1)
    }
  }

  @tailrec
  private def filter(found: Array[BitVector], p: Int = arity - 1, c: Boolean = false): Boolean =
    if (p < 0) c
    else {
      val ch = scope(p).dom.filter(i => found(p)(i))
      filter(found, p - 1, c || ch)
    }

  private var allTuples: List[Array[Int]] = tupleSet.toList

  private var _size: Int = tupleSet.size

  def save = (allTuples, size)

  def restore(d: (List[Array[Int]], Int)) {
    allTuples = d._1
    _size = d._2
  }

  override def checkIndices(t: Array[Int]) = tupleSet.check(t)

  def size = _size

  def filterTuples(f: Array[Int] => Boolean) {
    val oldSize = size
    allTuples = allTuples.filter(f)
    _size = allTuples.length
    if (size != oldSize) altering()
  }

  def tuples = allTuples

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

  def getEvaluation = arity * size

  def simpleEvaluation = math.min(7, scope.count(_.dom.size > 1))

  override def toString = scope.mkString("STR(", ", ", ")")
}
