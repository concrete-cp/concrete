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

import concrete.Contradiction
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Residues
import cspom.util.BitVector

final class ExtensionConstraintGeneral(
  var matrix: Matrix,
  shared: Boolean,
  scope: Array[Variable])
    extends ExtensionConstraint(scope) with Residues with ConflictCount {

  def removeTuple(tuple: Array[Int]) = {
    ??? // disEntail();
    residues.remove(tuple);
    set(tuple, false)
  }

  def removeTuples(base: Array[Int]) = ??? //tuples(base).count(removeTuple)

  override def revise(ps: ProblemState, mod: BitVector): Outcome = {

    val skip = this.skip(mod)
    val doms = ps.doms(scope)
    var cs = ps

    for (position <- 0 until arity) {
      if (position != skip && !supportCondition(doms, position)) {
        val nd = reviseDomain(doms, position)
        if (nd.isEmpty) {
          return Contradiction(Seq(scope(position)))
        } else if (nd ne doms(position)) {
          doms(position) = nd
          cs = cs.updateDomNonEmptyNoCheck(scope(position), nd)
        }
      }
    }
    cs.entailIfFree(this, doms)
  }
  //  
  //  override def reviseVariable(ps:ProblemState, position: Int, mod: List[Int]) = {
  //    if (supportCondition(domains, position)) {
  //      assert(super.reviseVariable(domains, position, mod) eq domains(position))
  //      domains(position)
  //    } else
  //      super.reviseVariable(domains, position, mod);
  //  }

  override def check(tuple: Array[Int]) = {
    matrix.check(tuple)
  }

  override def dataSize = matrix.size

}
