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

package concrete
package constraint
package extension

import bitvectors.BitVector

final class UnaryExt(scope: Variable, var matrix: Matrix)
  extends ExtensionConstraint(Array(scope)) {

  var allowed: Domain = _

  def init(ps: ProblemState): Outcome = {
    allowed = ps.dom(scope).filter { v => matrix.check(Array(v)) }
    ps.updateDom(scope, allowed).entail(this)
  }

  override def consistent(ps: ProblemState, mod: Traversable[Int]): Outcome =
    if (ps.dom(scope).disjoint(allowed)) ps else Contradiction(Nil)

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    logger.debug(s"Revision of unary constraint ${toString(ps)}")
    ps
  }

  def removeTuple(tuple: Array[Int]): Boolean = {
    ??? //disEntail();
    if (matrix.check(tuple)) {
      matrix.set(tuple, status = false)
      true
    } else false
  }

  def removeTuples(base: Array[Int]): Int = ???

  //  base match {
  //    case Array(-1) => scope.indices.count(i => removeTuple(Array(i)))
  //    case Array(i)  => if (removeTuple(Array(i))) 1 else 0
  //    case _         => throw new IllegalArgumentException()
  //  }

  override def check(tuple: Array[Int]) = matrix.check(tuple)

  def simpleEvaluation = 1

  def advise(ps: ProblemState, event: Event, p: Int) = -1

  override def dataSize = matrix.size

}
