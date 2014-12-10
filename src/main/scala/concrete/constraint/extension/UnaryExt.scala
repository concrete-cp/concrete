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
import concrete.Revised
import concrete.Variable

final class UnaryExt(scope: Variable, matrix: Matrix, shared: Boolean)
  extends ExtensionConstraint(Array(scope), matrix, shared) {

  type State = Unit

  def initState = Unit

  def revise(domains: IndexedSeq[Domain], state: State) =
    Revised(Vector(domains(0).filter { i => matrix.check(Array(i)) }), true)

  def removeTuple(tuple: Array[Int]) = {
    ??? //disEntail();
    unshareMatrix()
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
  def advise(domains: IndexedSeq[Domain], p: Int) = domains(0).size
  override def dataSize = matrix.size

}
