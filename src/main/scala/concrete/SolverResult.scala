package concrete

import java.util.Optional
import scala.collection.JavaConverters._

sealed trait SolverResult {
  def isSat: Boolean

  def get: Option[Map[Variable, Any]]

  def getInteger: java.util.Optional[java.util.Map[Variable, java.lang.Integer]] = {
    val o = getInt.map {
      _.mapValues(i => i: java.lang.Integer).asJava
    }

    Optional.ofNullable(o.orNull)
  }

  def getInt: Option[Map[Variable, Int]] = get.map { s =>
    s.mapValues(util.Math.any2Int)
  }
}

case class SAT(solution: Map[Variable, Any]) extends SolverResult {
  def isSat = true

  def get = Some(solution)

  override def toString: String = "SAT: " + (if (solution.size > 10) solution.take(10).toString + "..." else solution.toString)
}

case object UNSAT extends SolverResult {
  def isSat = false

  def get = None

  override def toString = "UNSAT"
}

object UNKNOWNResult {
  def apply(cause: Throwable): UNKNOWNResult = UNKNOWNResult(Some(cause))
}

case class UNKNOWNResult(cause: Option[Throwable]) extends SolverResult {
  def isSat = false

  def get = None

  override def toString: String = "UNKNOWN" + cause
}

case object RESTART extends SolverResult {
  def isSat = false

  def get = None

  override def toString = "RESTART"
}
