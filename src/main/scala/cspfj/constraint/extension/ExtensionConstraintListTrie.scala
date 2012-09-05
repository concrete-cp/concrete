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
import cspom.extension.HashTrie
import cspfj.UNSATException
import cspfj.constraint.Removals

final class ExtensionConstraintListTrie(_scope: Array[Variable], private val _tts: ListTrie)
  extends Constraint(_scope) with Loggable with Removals with Backtrackable[ListTrie] {

  //require(_tts.initialContent == false)

  private var trie = _tts

  private val found = scope map (p => BitVector.newBitVector(p.dom.maxSize))

  private var neverRevised = true

  override def setLvl(l: Int) {
    super.setLvl(l)
    setLevel(l)
  }

  override def restoreLvl(l: Int) {
    super.restoreLvl(l)
    restoreLevel(l)
  }

  def revise(mod: List[Int]) = {
    //fine("Revising " + this + " : " + mod.toList)
    found.foreach(_.fill(false))
    //println(trie.size)
    //val oldSize = trie.size

    val newTrie = trie.filterTrie(
      (i, v) => scope(i).dom.present(v), mod.reverse)

    if (newTrie ne trie) {
      if (newTrie.isEmpty) throw UNSATException.e

      trie = newTrie
      altering()
    }

    trie.setFound(found) //foreachTrie((i, v) => found(i).set(v))
    val c = filter()

    val card = scope.map(v => BigInt(v.dom.size)).product
    assert(card >= trie.size, card + " < " + trie.size + "!")
    if (card == trie.size) {
      //logger.info("Entailing " + this)
      entail()
    }

    c
  }

  @tailrec
  private def filter(p: Int = arity - 1, c: Boolean = false): Boolean =
    if (p < 0) c
    else {
      val ch = scope(p).dom.filter(i => found(p)(i))
      filter(p - 1, c || ch)
    }

  def save = trie

  def restore(d: ListTrie) {
    trie = d
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

  def getEvaluation = trie.size

  def simpleEvaluation = math.min(7, scope.count(_.dom.size > 1))

  override def toString = scope.mkString("Trie(", ", ", ")")
}
