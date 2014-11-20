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
import concrete.Domain
import concrete.util.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete.Variable
import concrete.UNSATException
import concrete.constraint.Removals
import cspom.Statistic
import concrete.UNSATObject
import concrete.Revised
import concrete.Contradiction

object ReduceableExt {
  @Statistic
  var fills = 0l
}

final class ReduceableExt(_scope: Array[Variable], val initState: Relation)
  extends Constraint(_scope) with LazyLogging with Removals {

  type State = Relation

  //println("sizesR " + arity + " " + trie.lambda + " " + trie.edges)

  private val unsupported = new Array[BitVector](arity)

  def revise(domains: IndexedSeq[Domain], mod: List[Int], trie: Relation) = {
    //println(this)
    //logger.fine("Revising " + this + " : " + mod.toList)
    for (i <- 0 until scope.length) {
      unsupported(i) = domains(i).toBitVector
    }
    //found.foreach(_.fill(false))

    //val oldSize = trie.size

    //println(this + ": filtering " + oldSize)

    val newTrie = trie.filterTrie(
      { (p, i) => domains(p).present(i) }, mod)

    //println("filtered " + newTrie.size)

    //logger.fine("Filtered from " + oldSize + " to " + newTrie.size)

    // val newSize = newTrie.size

    if (newTrie.isEmpty) {
      Contradiction
    } else {

      //assert(newSize <= oldSize)

      //sizes(domSizes)

      val unsup = newTrie
        .fillFound({ (depth: Int, i: Int) =>
          ReduceableExt.fills += 1
          unsupported(depth) -= i
          unsupported(depth).isEmpty
        }, arity)

      val c = (0 until arity).map(p => if (unsup(p)) domains(p).filter(v => !unsupported(p)(v)) else domains(p))

      //    val card = cardSize()
      //    assert(card < 0 || card >= trie.size, card + " < " + trie.size + "!")
      //    if (card == trie.size) {
      //      //logger.info("Entailing " + this)
      //      entail()
      //    }

      Revised(c, isFree(c), newTrie)

    }

  }

  override def check(t: Array[Int]) = initState.contains(t)

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

  val prop = initState.edges.toDouble / doubleCardSize(scope.map(_.initDomain))

  def getEvaluation(domains: IndexedSeq[Domain]) = (prop * doubleCardSize(domains)).toInt

  val simpleEvaluation = math.min(7, scope.count(_.initDomain.size > 1))

  override def toString = scope.mkString("ExtReduce(", ", ", ") / ") + initState.toString

  override def dataSize = initState.edges
}
