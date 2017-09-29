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
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Removals
import concrete.constraint.StatefulConstraint
import cspom.Statistic
import bitvectors.BitVector

object ReduceableExt {
  @Statistic
  var fills = 0l
}

final class ReduceableExt(scope: Array[Variable], val relation: Relation)
    extends Constraint(scope) with LazyLogging with Removals with StatefulConstraint[Relation] {

  require(scope.toSet.size == arity, "Variables must be distinct")

  override def init(ps: ProblemState) = ps.updateState(this, relation)

  //println("sizesR " + arity + " " + trie.lambda + " " + trie.edges)

  //private val newDomains = new Array[Domain](arity)

  def revise(ps: ProblemState, mod: BitVector) = {
    val domains = ps.doms(scope) //Array.tabulate(arity)(p => ps.dom(scope(p)))

    val trie = ps(this)
    //found.foreach(_.fill(false))

    //val oldSize = trie.size

    //println(this + ": filtering " + oldSize)

    logger.trace("Filtering with " + scope.toSeq.map(_.toString(ps)))

    val newTrie = trie.filterTrie(domains, mod.traversable.toList)

    //println("filtered " + newTrie.size)

    //logger.fine("Filtered from " + oldSize + " to " + newTrie.size)

    // val newSize = newTrie.size

    if (newTrie.isEmpty) {
      Contradiction(scope)
    } else {

      //assert(newSize <= oldSize)

      //sizes(domSizes)

      val newDomains = newTrie.supported(domains)

      var cs: ProblemState = ps.updateState(this, newTrie)
      for (p <- 0 until arity) {
        //println(s"$p: ${scope(p)}: ${domains(p)} -> ${newDomains(p)}")
        if (newDomains(p).size < domains(p).size) {
          cs = cs.updateDomNonEmptyNoCheck(scope(p), domains(p).filter(newDomains(p).contains)) //(!domains(p).present(_))
        }
      }
      cs.entailIfFree(this)
    }

  }

  override def check(t: Array[Int]) = {

    relation.contains(t)
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

  val prop = relation.edges.toDouble / scope.map(_.initDomain.size.toDouble).product

  def getEvaluation(ps: ProblemState) = (prop * doubleCardSize(ps)).toInt

  val simpleEvaluation = math.min(7, scope.count(_.initDomain.size > 1))

  override def dataSize = relation.edges
}
