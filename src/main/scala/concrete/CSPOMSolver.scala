package concrete

import cspom.variable.CSPOMConstant
import com.typesafe.scalalogging.LazyLogging
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq

class CSPOMSolver(
  val solver: Solver,
  val cspom: CSPOM,
  val variables: Map[CSPOMVariable[_], Variable]) extends Iterator[CSPOMSolution]
    with LazyLogging {

  def hasNext: Boolean = solver.hasNext

  def next() = new CSPOMSolution(cspom, variables, solver.next)

  def maximize(v: String) = cspom.variable(v) match {
    case Some(cv) => solver.maximize(variables(cv))
    case _        => logger.warn(s"$v is not a variable, nothing to maximize")
  }

  def minimize(v: String) = cspom.variable(v) match {
    case Some(cv) => solver.minimize(variables(cv))
    case _        => logger.warn(s"$v is not a variable, nothing to minimize")
  }

  //  def decisionVariables(dv: Seq[CSPOMExpression[_]]): Unit = {
  //    solver.decisionVariables(dv.collect {
  //      case e: CSPOMVariable[_] => variables(e)
  //    })
  //  }

  def optimizes: Option[CSPOMVariable[_]] = solver.optimises.map {
    case variable => variables.find(_._2 == variable).get._1
  }

  def concreteProblem = solver.problem

  def statistics = solver.statistics

  def solution(concreteSol: Map[Variable, Any]) = new CSPOMSolution(cspom, variables, concreteSol)

}

class CSPOMSolution(private val cspom: CSPOM, private val variables: Map[CSPOMVariable[_], Variable], private val concreteSol: Map[Variable, Any])
    extends Map[String, Any]
    with LazyLogging {

  lazy val apply = concrete2CspomSol(concreteSol)

  private def concrete2CspomSol(sol: Map[Variable, Any]): Map[String, Any] = {
    val cspomsol = cspom.expressionsWithNames.iterator
      .flatMap {
        case (n, e) => concrete2CspomSol(n, e, sol)
      }
      .toMap

    //logger.info("CSPOM solution: " + cspomsol.toSeq.sortBy(_._1))
    cspomsol
  }

  private def concrete2CspomSol(name: String, expr: CSPOMExpression[_], sol: Map[Variable, Any]): Seq[(String, Any)] = {
    expr match {
      case seq: CSPOMSeq[_] =>
        (name -> seq.values.map(v => concrete2CspomSol(name, v, sol))) +:
          seq.withIndex.flatMap {
            case (v, i) => concrete2CspomSol(s"$name[$i]", v, sol)
          }
      case const: CSPOMConstant[_]    => Seq(name -> const.value)
      case variable: CSPOMVariable[_] => Seq(name -> sol(variables(variable)))
    }
  }

  def get[T](key: CSPOMExpression[T]): Option[T] = {
    key match {
      case const: CSPOMConstant[T] => Some(const.value)
      case variable: CSPOMVariable[T] =>
        variables
          .get(variable)
          .map(concreteSol)
          .map(_.asInstanceOf[T])
      case _ => throw new AssertionError(s"Cannot obtain solution for $key")
    }
  }

  def apply[T](key: CSPOMExpression[T]): T = get(key).get
  //def apply(key: String): Any = get(key).get

  def +[B1 >: Any](kv: (String, B1)): Map[String, B1] = throw new UnsupportedOperationException
  def -(key: String): Map[String, Any] = throw new UnsupportedOperationException
  def get(key: String): Option[Any] = apply.get(key)
  def iterator: Iterator[(String, Any)] = apply.iterator

}