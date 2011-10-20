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

  private val toFind = initFound;

  private def initFound =
    scope map (v => BitVector.newBitVector(v.dom.maxSize, false))

  override def level_=(l: Int) {
    super.level = l
    dynamic.level = l
    if (level <= 0) {
      val itr = dynamic.iterator
      for (tuple <- itr if !dynamic.isTrue(tuple)) itr.remove(-1)
    }
  }

  def revise(revisator: RevisionHandler, reviseCount: Int) = {
    (scope, toFind).zipped.foreach { (v, tF) =>
      tF.fill(false)
      for (i <- v.dom.indices) tF.set(i)
    }

    val itr = dynamic.iterator
    for (tuple <- itr) {
      if (controlTuplePresence(tuple)) {
        (toFind, tuple).zipped.foreach((tF, i) => tF.clear(i))
      } else {
        itr.remove();
      }
    }

    filter(toFind, revisator);
  }

  private def filter(toFinds: Array[BitVector], revisator: RevisionHandler): Boolean = {
    (scope, toFinds).zipped.foreach { (v, tF) =>

      var rev = false;
      var i = tF.nextSetBit(0)
      while (i >= 0) {
        v.dom.remove(i);
        rev = true;
        i = tF.nextSetBit(i + 1)
      }
      if (rev) {
        if (v.dom.size <= 0) {
          return false;
        }
        revisator.revised(this, v);
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
    dynamic.unshareMatrix();
    var removed = 0;
    val itr = dynamic.hsIterator
    for (t <- JavaConversions.asScalaIterator(itr) if matches(t, base)) {
      // logger.fine("Removing " + Arrays.toString(currentTuple));
      itr.remove();
      assert(!dynamic.isTrue(t));
      removed += 1;
    }
    removed;
  }

  def matrixManager = dynamic

  def getEvaluation = arity * dynamic.size

}
