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

import scala.annotation.tailrec
import cspfj.constraint.Constraint
import cspfj.util.Backtrackable
import cspfj.util.BitVector
import cspfj.util.Loggable
import cspfj.Variable
import cspfj.UNSATException
import cspfj.constraint.Removals
import cspfj.Statistic

object ReduceableExt {
  @Statistic
  var fills = 0l
}

final class ReduceableExt(_scope: Array[Variable], private val _tts: Relation)
  extends Constraint(_scope) with Loggable with Removals with Backtrackable[Relation] {

  var trie = _tts

  private val unsupported = scope map (p => BitVector.newBitVector(p.dom.maxSize))

  override def setLvl(l: Int) {
    super.setLvl(l)
    setLevel(l)
  }

  override def restoreLvl(l: Int) {
    super.restoreLvl(l)
    restoreLevel(l)
  }

  def revise(mod: List[Int]) = {
    //logger.fine("Revising " + this + " : " + mod.toList)
    for (i <- scope.indices) {
      unsupported(i).fill(false)
      for (j <- scope(i).dom.indices) {
        unsupported(i).set(j)
      }
    }
    //found.foreach(_.fill(false))

    val rev = mod.reverse

    //val oldSize = trie.size

    //println(this + ": filtering " + oldSize)

    val newTrie = trie.filterTrie(
      { (p, i) => scope(p).dom.present(i) }, rev)

    //println("filtered " + newTrie.size)

    //logger.fine("Filtered from " + oldSize + " to " + newTrie.size)

    // val newSize = newTrie.size

    if (newTrie.isEmpty) { throw UNSATException.e }

    //assert(newSize <= oldSize)

    if (newTrie ne trie) {
      trie = newTrie
      altering()
    }

    //sizes(domSizes)

    val c = trie.
      fillFound({ (depth: Int, i: Int) =>
        ReduceableExt.fills += 1
        unsupported(depth).clear(i) && unsupported(depth).isEmpty
      }, arity).
      filter(p => scope(p).dom.filter(i => !unsupported(p)(i)))

    //    val card = cardSize()
    //    assert(card < 0 || card >= trie.size, card + " < " + trie.size + "!")
    //    if (card == trie.size) {
    //      //logger.info("Entailing " + this)
    //      entail()
    //    }
    if (isFree) {
      entail()
    }

    c
  }

  def save = {
    //println(this + " <- " + trie.size)
    trie
  }

  def restore(d: Relation) {
    trie = d
    //println(this + " -> " + trie.size)
  }

  override def checkIndices(t: Array[Int]) = trie.contains(t)

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

  val prop = _tts.edges.toDouble / doubleCardSize

  def getEvaluation = (prop * doubleCardSize).toInt

  def simpleEvaluation = math.min(7, scope.count(_.dom.size > 1))

  override def toString = scope.mkString("ExtReduce(", ", ", ")")
}
