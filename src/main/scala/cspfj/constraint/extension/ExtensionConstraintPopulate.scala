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
import cspfj.Statistic

final class ExtensionConstraintPopulate(_scope: Array[Variable], private val trie: MDD)
  extends Constraint(_scope) with Removals with Backtrackable[Set[MDD]] {

  private val found = scope map (p => BitVector.newBitVector(p.dom.maxSize))

  var no = Set[MDD]()

  override def setLvl(l: Int) {
    super.setLvl(l)
    setLevel(l)
  }

  override def restoreLvl(l: Int) {
    super.restoreLvl(l)
    restoreLevel(l)
  }

  val domSizes = new Array[Int](arity)

  def revise(mod: List[Int]) = {
    //logger.fine("Revising " + this + " : " + mod.toList)
    found.foreach(_.fill(false))

    //val oldSize = trie.size

    val rev = mod.reverse

    //println(this + ": filtering " + oldSize)

    no = trie.populate(
      { (p, i) => ExtensionConstraintReduceable.checks += 1; scope(p).dom.present(i) }, rev, no)

    altering()
    //println("filtered " + newTrie.size)
    //    val newTrie = trie.filter(scope, mod.reverse)

    //logger.fine("Filtered from " + oldSize + " to " + newTrie.size)

    sizes(domSizes)

    val notFound = trie.fillFoundPop({ (depth: Int, i: Int) =>
      ExtensionConstraintReduceable.fills += 1
      found(depth).set(i) && {
        domSizes(depth) -= 1
        assert(domSizes(depth) == scope(depth).dom.size - found(depth).cardinality)
        domSizes(depth) == 0
      }
    }, arity, no)

    val c = filter(notFound)

    val card = cardSize()
    assert(card < 0 || card >= trie.size, card + " < " + trie.size + "!")
    if (card == trie.size) {
      //logger.info("Entailing " + this)
      entail()
    }

    c
  }

  private def filter(notFound: Traversable[Int]) = notFound.foldLeft(false)(
    (acc, p) => scope(p).dom.filter(i => found(p)(i)) || acc)

  def save = {
    //println(this + " <- " + trie.size)
    no
  }

  def restore(d: Set[MDD]) {
    no = d
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

  def getEvaluation = trie.size

  def simpleEvaluation = math.min(7, scope.count(_.dom.size > 1))

  override def toString = scope.mkString("ExtReduce(", ", ", ")")
}
