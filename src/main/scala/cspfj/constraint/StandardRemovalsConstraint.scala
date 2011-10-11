package cspfj.constraint

import cspfj.problem.Variable
import scala.collection.immutable.Map
import cspfj.filter.RevisionHandler

trait StandardRemovalsConstraint extends Constraint {

  private var removals = 0;

  def setRemovals(position: Int, value: Int) { removals = value }

  def fillRemovals(value: Int) { removals = value }

  def hasNoRemovals(reviseCount: Int) = removals < reviseCount

}