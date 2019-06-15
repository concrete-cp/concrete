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

package concrete.constraint.extension

;

import bitvectors.BitVector
import concrete._
import concrete.constraint.Residues

final class ExtensionConstraintGeneral(
                                        var matrix: Matrix,
                                        shared: Boolean,
                                        scope: Array[Variable])
  extends ExtensionConstraint(scope) with Residues with ConflictCount {

  def removeTuple(tuple: Array[Int]): Boolean = {
    ??? // disEntail();
    residues.remove(tuple)
    set(tuple, status = false)
  }

  def removeTuples(base: Array[Int]): Int = ??? //tuples(base).count(removeTuple)

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {

    val skip = this.skip(mod)
    val doms = ps.doms(scope)

    ps.fold(0 until arity) { case (cs, p) =>
      if (p == skip || supportCondition(doms, p)) {
        cs
      } else {
        val nd = reviseDomain(doms, p)
        doms(p) = nd
        cs.updateDom(scope(p), nd)
      }
    }
      .entailIfFree(this, doms)
  }

  //
  //  override def reviseVariable(ps:ProblemState, position: Int, mod: List[Int]) = {
  //    if (supportCondition(domains, position)) {
  //      assert(super.reviseVariable(domains, position, mod) eq domains(position))
  //      domains(position)
  //    } else
  //      super.reviseVariable(domains, position, mod);
  //  }

  override def check(tuple: Array[Int]): Boolean = {
    matrix.check(tuple)
  }

  override def dataSize: Int = matrix.size

  override def advise(problemState: ProblemState, event: Event, pos: Int): Int = advise(problemState, pos)

}
