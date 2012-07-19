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

  def revise(modified: Seq[Int]) = {
    //fine("Revising " + this + " : " + mod.toList)
    found.foreach(_.fill(false))

    val mod = modified.toList

    var i = 0
    while (i < bound) {
      if (controlTuplePresence(tuplesArray(i), mod)) {
        setFound(tuplesArray(i), found)
        i += 1
      } else {
        altering()
        bound -= 1
        val tmp = tuplesArray(i)
        tuplesArray(i) = tuplesArray(bound)
        tuplesArray(bound) = tmp
      }
    }

    val c = filter(found)

    val card = scope.map(v => BigInt(v.dom.size)).product
    assert(card >= bound, card + " < " + bound + "!")
    if (card == bound) {
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

  private var bound: Int = tuplesArray.length

  def save = bound

  def restore(d: Int) {
    bound = d
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
