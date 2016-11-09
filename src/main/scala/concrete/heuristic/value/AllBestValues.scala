package concrete.heuristic.value

import concrete.ParameterManager
import concrete.Variable
import concrete.Problem
import concrete.Domain
import scala.collection.mutable.AbstractSet
import scala.collection.generic.GenericCompanion
import scala.deprecatedOverriding
import scala.collection.generic._

import scala.collection.AbstractIterator
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashEntry
import scala.collection.mutable.SetLike
import scala.collection.mutable.HashTable
import scala.collection.mutable.ListBuffer

class AllBestValues(fallback: ValueHeuristic) extends ValueHeuristic {

  def this(params: ParameterManager) = this{
    val valueHeuristicClass: Class[_ <: ValueHeuristic] =
      params.classInPackage("bestvalue.fallback", "concrete.heuristic.value", classOf[RandomBound])

    valueHeuristicClass.getConstructor(classOf[ParameterManager]).newInstance(params)
  }

  override def toString = "best";

  def compute(p: Problem) {
    // Nothing to compute
  }

  override def selectIndex(variable: Variable, domain: Domain) = {
    BestValue.best.getOrElse(variable, Nil).find(domain.present).getOrElse(fallback.selectIndex(variable, domain))
  }

  def shouldRestart = false
}