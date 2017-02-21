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

import scala.annotation.varargs
import scala.collection.JavaConverters._

import concrete.constraint.Constraint

object Problem {
  @varargs
  def apply(vars: Variable*) = new Problem(vars.toArray)
}

final class Problem(val variables: Array[Variable]) {
  //require(variables.nonEmpty, "A problem with no variables makes no sense")
  require(variables.map(_.name).distinct.size == variables.size, "Duplicates in variable names")

  val nbVariables = variables.foldLeft(0) {
    (acc, v) => v.id = acc; acc + 1
  }

  val variableMap: Map[String, Variable] = variables.map(v => v.name -> v).toMap
  var constraints: Array[Constraint] = Array()

  private var _nextCId = 0

  def addConstraint(constraint: Constraint): Problem = addConstraints(Seq(constraint))

  def addConstraints(cs: Seq[Constraint]): Problem = {
    constraints ++= cs

    for (c <- cs) {
      _nextCId = c.identify(_nextCId)
      for (p <- 0 until c.arity) {
        val v = c.scope(p)
        val pc = v.addConstraint(c)
        c.positionInVariable(p) = pc
      }
    }

    this

  }

  def initState = ProblemState(this)

  def toString(state: ProblemState) = {
    variables.map(_.toString(state)).mkString("\n") + "\n" +
      constraints.iterator
      .map { c =>
        c.id + ". " + (
          if (state.entailed.hasInactiveVar(c)) c.toString(state) + " [entailed]" else c.toString(state))
      }
      .mkString("\n") + "\n" + stats(state)
  }

  override def toString = toString(initState.toState)

  def stats(state: ProblemState) = {
    val entailed = -1 //state.entailed.cardinality

    s"Total ${variables.size} variables, ${constraints.size - entailed} active constraints and $entailed entailed constraints"
  }

  //val maxDomainSize = variables.iterator.map(_.dom.size).max

  def maxCId = _nextCId - 1
  // def nd = variables.iterator.map(_.dom.size).sum
  def variable(name: String) = variableMap(name)

  def getVariables = variables.toSeq.asJava

}
