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
import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.UNSATException
import concrete.Variable
import concrete.constraint.Residues

final class ExtensionConstraintGeneral(
  _matrix: Matrix,
  shared: Boolean,
  scope: Array[Variable])
  extends ConflictCount(scope, _matrix, shared) with Residues {

  def removeTuple(tuple: Array[Int]) = {
    ??? // disEntail();
    residues.remove(tuple);
    set(tuple, false)
  }

  def removeTuples(base: Array[Int]) = ??? //tuples(base).count(removeTuple)

  override def revise(ps: ProblemState, mod: List[Int]): Outcome = {
    val skip = this.skip(mod)
    var cs = ps
    for (position <- 0 until arity) {
      if (position != skip && !supportCondition(cs, position)) {
        cs.updateDom(scope(position), reviseDomain(cs, position)) match {
          case Contradiction   => return Contradiction
          case s: ProblemState => cs = s
        }
      }
    }
    cs.entailIfFree(this)
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

  override def dataSize = _matrix.size
}
