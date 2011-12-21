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

package cspfj.problem;

import scala.collection.mutable.MultiMap
import scala.collection.JavaConversions

import cspfj.constraint.Constraint

final class Problem {
  private var variableMap: Map[String, Variable] = Map.empty
  private var _variables: IndexedSeq[Variable] = IndexedSeq.empty
  private var _constraints: List[Constraint] = Nil

  private var _maxDomainSize = -1
  private var _maxArity = 0
  private var _maxVId = 0
  private var _maxCId = 0
  private var _currentLevel = 0

  def addVariable(name: String, domain: Domain) = {
    require(!variableMap.contains(name), "A variable named " + name + " already exists");

    val variable = new Variable(name, domain);
    variableMap += name -> variable
    _variables :+= variable

    _maxVId = math.max(variable.getId, _maxVId)
    variable;
  }

  def addConstraint(constraint: Constraint) {
    _constraints ::= constraint;
    _maxArity = math.max(_maxArity, constraint.arity)
    _maxCId = math.max(_maxCId, constraint.getId)
    constraint.scope foreach (v => v.addConstraint(constraint))
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
    _variables.foreach(_.dom.setLevel(level))
    _constraints.foreach(_.setLevel(level))
  }

  private def restoreLevel(level: Int) {
    _variables.foreach(_.dom.restoreLevel(level))
    _constraints.foreach(_.restoreLevel(level))
  }

  def reset() {
    _currentLevel = 0;
    _variables.foreach(_.dom.reset())
    _constraints.foreach(_.restoreLevel(0))
  }

  override def toString = {
    val entailed = _constraints.count(_.isEntailed)

    _variables.mkString("\n") + "\n" + _constraints.filter(!_.isEntailed).mkString("\n") + "\n" +
      "Total " + _variables.size + " variables, " +
      (_constraints.size - entailed) + " active constraints and " +
      entailed + " entailed constraints"
  }

  def maxDomainSize = {
    if (_maxDomainSize < 0) {
      _maxDomainSize = variables map { _.dom.size } max
    }
    _maxDomainSize
  }

  def currentLevel = _currentLevel
  def maxArity = _maxArity
  def maxVId = _maxVId
  def maxCId = _maxCId
  def nd = _variables map { _.dom.size } sum
  def variable(name: String) = variableMap(name)
  def variables = _variables
  def constraints = _constraints
  def getVariables = JavaConversions.seqAsJavaList(variables)

}
