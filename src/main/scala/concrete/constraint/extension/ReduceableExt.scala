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

import com.typesafe.scalalogging.LazyLogging
import concrete.Contradiction
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Removals
import cspom.Statistic
import concrete.constraint.StatefulConstraint

object ReduceableExt {
  @Statistic
  var fills = 0l
}

final class ReduceableExt(_scope: Array[Variable], val relation: Relation)
  extends Constraint(_scope) with LazyLogging with Removals with StatefulConstraint {

  type State = Relation
  
  override def init(ps:ProblemState) = ps.updateState(id, relation)

  //println("sizesR " + arity + " " + trie.lambda + " " + trie.edges)

  def revise(ps: ProblemState, mod: List[Int]) = {
    val domains = ps.domains(scope).toArray
    val unsupported = domains.map(_.to[collection.mutable.Set])
    val trie = ps(this)
    //found.foreach(_.fill(false))

    //val oldSize = trie.size

    //println(this + ": filtering " + oldSize)

    logger.debug("Filtering with " + _scope.toSeq.map(_.toString(ps)))

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

      var cs: Outcome = ps
      for (p <- 0 until arity) {
        if (unsup(p)) cs = cs.filterDom(_scope(p))(!unsupported(p)(_))
      }
      cs.updateState(id, newTrie).entailIfFree(this)
    }

  }

  override def check(t: Array[Int]) = relation.contains(t)

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

  val prop = relation.edges.toDouble / scope.map(_.initDomain.size.toDouble).product

  def getEvaluation(ps: ProblemState) = (prop * doubleCardSize(ps)).toInt

  val simpleEvaluation = math.min(7, scope.count(_.initDomain.size > 1))

  override def dataSize = relation.edges
}
