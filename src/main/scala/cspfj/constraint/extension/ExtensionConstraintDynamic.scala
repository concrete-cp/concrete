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

import cspfj.constraint.AbstractConstraint
import cspfj.problem.Variable
import cspfj.util.BitVector
import cspfj.filter.RevisionHandler
import scala.collection.JavaConversions
import cspfj.constraint.SimpleRemovals

final class ExtensionConstraintDynamic(
  scope: Array[Variable], matrix: TupleSet, shared: Boolean) extends AbstractConstraint(scope)
  with SimpleRemovals
  with ExtensionConstraint {

  private val dynamic = new MatrixManagerDynamic(scope, matrix, shared, tuple)

  private val toFind =
    scope map (v => BitVector.newBitVector(v.dom.maxSize, false))

  override def level_=(l: Int) {
    super.level = l
    dynamic.level = l
    //    if (level <= 0) {
    //      val itr = dynamic.iterator
    //      for (tuple <- itr if !dynamic.isTrue(tuple)) itr.remove(-1)
    //    }
  }

  def revise(revisator: RevisionHandler, reviseCount: Int) = {
    var found: Set[(Variable, Int)] = Set.empty

    val itr = dynamic.iterator
    for (tuple <- itr) {
      if (controlTuplePresence(tuple)) {
        found ++= scope.zip(tuple)
      } else {
        itr.remove();
      }
    }

    filter(found, revisator);
  }

  private def filter(found: Set[(Variable, Int)], revisator: RevisionHandler): Boolean = {
    for (v <- scope) {
      var rev = false
      for (i <- v.dom.indices if !found((v, i))) {
        v.dom.remove(i)
        rev = true
      }
      if (rev) {
        if (v.dom.size <= 0) return false
        revisator.revised(this, v)
      }
    }

    true
  }

  def check = dynamic.check

  def removeTuple(tuple: Array[Int]) = dynamic.removeTuple(tuple)

  private def matches(tuple: Array[Int], base: Array[Int]) = {
    assert(tuple.length == base.length);
    (base, tuple).zipped.forall { (b, t) => b < 0 || b == t }
  }

  def removeTuples(base: Array[Int]) = {
    throw new UnsupportedOperationException
//    dynamic.unshareMatrix();
//    var removed = 0;
//    val itr = dynamic.hsIterator
//    for (t <- JavaConversions.asScalaIterator(itr) if matches(t, base)) {
//      // logger.fine("Removing " + Arrays.toString(currentTuple));
//      itr.remove();
//      assert(!dynamic.isTrue(t));
//      removed += 1;
//    }
//    removed;
  }

  def matrixManager = dynamic

  def getEvaluation = arity * dynamic.size
  
  override def toString = arity + "-ary STR w/ " + dynamic

}
