package concrete


import com.typesafe.scalalogging.LazyLogging
import cspom.{ExpressionMap, StatisticsManager}
import cspom.variable.{CSPOMConstant, CSPOMExpression, CSPOMSeq, CSPOMVariable}


class CSPOMSolver(
                   val solver: Solver,
                   val cspom: ExpressionMap,
                   val variables: Map[CSPOMVariable[_], Variable]) extends Iterator[CSPOMSolution]
  with LazyLogging {

  def hasNext: Boolean = solver.hasNext

  def next() = new CSPOMSolution(cspom, variables, solver.next())

  def optimizes: Option[CSPOMVariable[_]] = solver.optimises.map {
    variable => variables.find(_._2 == variable).get._1
  }

  def concreteProblem: Problem = solver.problem

  def statistics: StatisticsManager = solver.statistics

  def solution(concreteSol: Map[Variable, Any]) = new CSPOMSolution(cspom, variables, concreteSol)
}

class CSPOMSolution(private val cspom: ExpressionMap, private val variables: Map[CSPOMVariable[_], Variable], private val concreteSol: Map[Variable, Any])
  extends Map[String, Any]
    with LazyLogging {

  // lazy val apply = concrete2CspomSol(concreteSol)

  def get(key: Variable): Option[Any] = concreteSol.get(key)

  def apply(key: CSPOMExpression[_]): Any = get(key).get

  def get(key: CSPOMExpression[_]): Option[Any] = {
    key match {
      case const: CSPOMConstant[_] => Some(const.value)
      case variable: CSPOMVariable[_] =>
        variables
          .get(variable)
          .map(concreteSol)
      case seq: CSPOMSeq[_] =>
        Some(seq.values.map(this(_)))
      case _ => throw new AssertionError(s"Cannot obtain solution for $key")
    }
  }

  def +[B1 >: Any](kv: (String, B1)): Map[String, B1] = throw new UnsupportedOperationException

  def -(key: String): Map[String, Any] = throw new UnsupportedOperationException

  //def apply(key: String): Any = get(key).get

  def get(key: String): Option[Any] = {
    cspom.expression(key)
      .orElse {
        throw new IllegalArgumentException(s"$key not in $this")
      }
      .flatMap(e => get(e))
  }

  def iterator: Iterator[(String, Any)] = cspom.expressionsWithNames.flatMap {
    case (n, e) => concrete2CspomSol(n, e)
  }

  //  private def concrete2CspomSol(sol: Map[Variable, Any]): Map[String, Any] = {
  //    val cspomsol = cspom.expressionsWithNames
  //      .flatMap {
  //        case (n, e) => concrete2CspomSol(n, e, sol)
  //      }
  //      .toMap
  //
  //    //logger.info("CSPOM solution: " + cspomsol.toSeq.sortBy(_._1))
  //    cspomsol
  //  }

  private def concrete2CspomSol(name: String, expr: CSPOMExpression[_]): Seq[(String, Any)] = {
    expr match {
      case seq: CSPOMSeq[_] =>
        (name -> seq.values.map(v => concrete2CspomSol(name, v))) +:
          seq.withIndex.flatMap {
            case (v, i) => concrete2CspomSol(s"$name[$i]", v)
          }
      case const: CSPOMConstant[_] => Seq(name -> const.value)
      case variable: CSPOMVariable[_] => Seq(name -> concreteSol(variables(variable)))
    }
  }

}