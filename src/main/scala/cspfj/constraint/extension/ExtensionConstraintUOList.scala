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
import cspfj.util.UOList

final class ExtensionConstraintUOList(
  scope: Array[Variable],
  private var allTuples: List[Array[Int]])
  extends ExtensionConstraint(scope, new TupleSeq(allTuples), false)
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

 
    var newList: List[Array[Int]] = Nil
    var oldList = allTuples
    var newSize = 0
    while (oldList.nonEmpty) {
      val tuple = oldList.head
      if (controlTuplePresence(tuple, modified)) {
        setFound(tuple, found)
        newList ::= tuple
        newSize += 1
      }
      oldList = oldList.tail
    }

    if (nbTuples != newSize) {
      allTuples = newList
      nbTuples = newSize
      altering()
    }

    val c = filter(found)

    val card = scope.map(v => BigInt(v.dom.size)).product
    assert(card >= nbTuples, card + " < " + nbTuples + "!")
    if (card == nbTuples) {
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

  private var nbTuples: Int = allTuples.size

  def save = (allTuples, nbTuples)

  def restore(d: (List[Array[Int]], Int)) {
    allTuples = d._1
    nbTuples = d._2
  }

  def filterTuples(f: Array[Int] => Boolean) {
    val oldSize = nbTuples
    allTuples = allTuples.filter(f)
    nbTuples = allTuples.length
    if (nbTuples != oldSize) altering()
  }

  def tuples = allTuples

  private def matches(tuple: Array[Int], base: Array[Int]) = {
    assert(tuple.length == base.length);
    (base, tuple).zipped.forall { (b, t) => b < 0 || b == t }
  }

  def removeTuples(base: Array[Int]) = throw new UnsupportedOperationException

  def removeTuple(t: Array[Int]) = throw new UnsupportedOperationException

  def getEvaluation = arity * nbTuples

  def simpleEvaluation = math.min(7, scope.count(_.dom.size > 1))

  override def toString = scope.mkString("STR(", ", ", ")")
}
