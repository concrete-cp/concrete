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

import concrete.constraint.Constraint
import scala.collection.JavaConversions
import scala.collection.mutable.ArrayBuffer
import java.util.Arrays
import scala.collection.immutable.VectorBuilder
import concrete.constraint.StatefulConstraint
import scala.annotation.varargs
import scala.collection.immutable.BitSet
import concrete.util.BitVector

object Problem {
  @varargs
  def apply(vars: Variable*) = new Problem(vars.toList)
}

final class Problem(val variables: List[Variable]) {
  //require(variables.nonEmpty, "A problem with no variables makes no sense")
  require(variables.map(_.name).distinct.size == variables.size, "Duplicates in variable names")

  val nbVariables = variables.foldLeft(0) {
    (acc, v) => v.id = acc; acc + 1
  }

  var decisionVariables: List[Variable] = variables

  val variableMap: Map[String, Variable] = variables.map(v => v.name -> v).toMap
  var constraints: Array[Constraint] = Array()

  private var _maxArity = 0
  private var _maxCId = 0

  def addConstraint(constraint: Constraint): Unit = addConstraints(Seq(constraint))

  def addConstraints(cs: Seq[Constraint]): Unit = {
    val buffer: ArrayBuffer[Constraint] = new ArrayBuffer()
    val hint = constraints.length + cs.size
    buffer.sizeHint(hint)
    buffer ++= constraints

    for (c <- cs) {
      val i = buffer.size
      buffer += c
      c.id = i
      _maxArity = math.max(_maxArity, c.arity)
      c.scope.foreach(v => v.addConstraint(c))
    }

    constraints = buffer.toArray
  }

  def initState = ProblemState(this)

  def toString(state: ProblemState) = {
    variables.map(_.toString(state)).mkString("\n") + "\n" +
      constraints.iterator
      .map { c =>
        if (state.isEntailed(c)) c.toString(state) + " [entailed]" else c.toString(state)
      }
      .mkString("\n") + "\n" + stats(state)
  }

  def stats(state: ProblemState) = {
    val entailed = state.entailed.cardinality

    s"Total ${variables.size} variables, ${constraints.size - entailed} active constraints and $entailed entailed constraints"
  }

  //val maxDomainSize = variables.iterator.map(_.dom.size).max

  def maxArity = _maxArity

  def maxCId = _maxCId
  // def nd = variables.iterator.map(_.dom.size).sum
  def variable(name: String) = variableMap(name)

  def getVariables = JavaConversions.seqAsJavaList(variables)

}
