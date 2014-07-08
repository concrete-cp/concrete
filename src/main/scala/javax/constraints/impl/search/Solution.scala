package javax.constraints.impl.search

import com.typesafe.scalalogging.LazyLogging
import concrete.Variable

class Solution(sol: Map[Variable, Int]) extends javax.constraints.Solution with LazyLogging {

  private lazy val namedSol = sol.map {
    case (k, v) => k.name -> v
  }

  def getAt(x$1: String): Int = ???
  def getMax(x$1: Int): Int = ???
  def getMax(x$1: String): Int = ???
  def getMin(x$1: Int): Int = ???
  def getMin(x$1: String): Int = ???
  def getNumberOfVarReals(): Int = ???
  def getNumberOfVarSets(): Int = ???
  def getNumberOfVars(): Int = ???
  def getProblem(): javax.constraints.Problem = ???
  def getRealValue(x$1: Int): Double = ???
  def getRealValue(x$1: String): Double = ???
  def getSolutionNumber(): Int = ???
  def getSolver(): javax.constraints.Solver = ???
  def getValue(x$1: Int): Int = ???
  def getValue(name: String): Int = namedSol(name)
  def isBound(): Boolean = ???
  def isBound(x$1: Int): Boolean = ???
  def isBound(x$1: String): Boolean = ???
  def log(i: Int) {
    log()
  }
  def log() {
    logger.info(sol.toString)
  }
  def setSolutionNumber(x$1: Int): Unit = ???
}