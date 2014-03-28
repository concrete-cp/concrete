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

object Problem {
  @annotation.varargs
  def apply(variables: Variable*) = new Problem(variables.toList)
}

final class Problem(val variables: List[Variable]) {
  //require(variables.nonEmpty, "A problem with no variables makes no sense")
  require(variables.map(_.name).distinct.size == variables.size, "Duplicates in variable names")

  def this(variables: Variable*) = this(variables.toList)

  val nbVariables = variables.foldLeft(0) {
    (acc, v) => v.getId = acc; acc + 1
  }

  val variableMap: Map[String, Variable] = variables.map(v => v.name -> v).toMap
  private var _constraints: List[Constraint] = Nil

  private var _maxDomainSize = -1
  private var _maxArity = 0
  private var _maxCId = 0
  private var _currentLevel = 0

  //  def addVariable(name: String, domain: Domain) = {
  //    require(!variableMap.contains(name), "A variable named " + name + " already exists");
  //
  //    val variable = new Variable(name, domain);
  //    variableMap += name -> variable
  //    _variables += variable
  //
  //    _maxVId = math.max(variable.getId, _maxVId)
  //    //_maxDomainSize = math.max(variable.dom.size, _maxDomainSize)
  //    variable;
  //  }
  //
  //  def removeVariable(v: Variable) {
  //    require(v.constraints.isEmpty)
  //
  //    variableMap -= v.name
  //    _variables -= v
  //    _maxVId = _variables map (_.getId) max
  //
  //  }

  def addConstraint(constraint: Constraint) {
    _constraints ::= constraint;
    _maxArity = math.max(_maxArity, constraint.arity)
    _maxCId = math.max(_maxCId, constraint.getId)
    constraint.scope foreach (v => v.addConstraint(constraint))
    constraint.inCN = true
  }

  def push() {
    _currentLevel += 1;
    setLevel(_currentLevel);

  }

  def pop() {
    assert(currentLevel > 0)
    _currentLevel -= 1;
    restoreLevel(_currentLevel);

  }

  private def setLevel(level: Int) {
    variables.foreach(_.dom.setLevel(level))
    _constraints.foreach(_.setLvl(level))
  }

  private def restoreLevel(level: Int) {
    variables.foreach(_.dom.restoreLevel(level))
    _constraints.foreach(_.restoreLvl(level))
  }

  def reset() {
    _currentLevel = 0;
    variables.foreach(_.dom.restoreLevel(0))
    _constraints.foreach(_.restoreLvl(0))
  }

  override def toString = {
    variables.map(_.toString).sorted.mkString("\n") + "\n" +
      _constraints.filter(!_.isEntailed).map(_.toString).sorted.mkString("\n") + "\n" +
      stats
  }

  def stats = {
    val entailed = _constraints.count(_.isEntailed)

    s"Total ${variables.size} variables, ${_constraints.size - entailed} active constraints and $entailed entailed constraints"
  }

  def maxDomainSize = {
    if (_maxDomainSize < 0 && variables.nonEmpty) {
      _maxDomainSize = variables map { _.dom.size } max
    }
    _maxDomainSize
  }

  def currentLevel = _currentLevel
  def maxArity = _maxArity

  def maxCId = _maxCId
  def nd = variables map { _.dom.size } sum
  def variable(name: String) = variableMap(name)

  def constraints = _constraints
  def getVariables = JavaConversions.seqAsJavaList(variables)

}
