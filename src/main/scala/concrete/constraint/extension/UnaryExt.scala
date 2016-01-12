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

import concrete.Domain
import concrete.Variable
import concrete.ProblemState

final class UnaryExt(scope: Variable, var matrix: Matrix)
    extends ExtensionConstraint(Array(scope)) {

  def init(ps: ProblemState) = ps.filterDom(scope)(v => matrix.check(Array(v))).entail(this)

  def revise(ps: ProblemState) = {
    assert(ps.isEntailed(this))
    ps
  }

  def removeTuple(tuple: Array[Int]) = {
    ??? //disEntail();
    if (matrix.check(tuple)) {
      matrix.set(tuple, false)
      true
    } else false
  }

  def removeTuples(base: Array[Int]) = ???

  //  base match {
  //    case Array(-1) => scope.indices.count(i => removeTuple(Array(i)))
  //    case Array(i)  => if (removeTuple(Array(i))) 1 else 0
  //    case _         => throw new IllegalArgumentException()
  //  }

  override def check(tuple: Array[Int]) = matrix.check(tuple)

  def simpleEvaluation = 1
  def advise(ps: ProblemState, p: Int) = ps.dom(scope).size
  override def dataSize = matrix.size

}
