package concrete.generator.cspompatterns
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import scala.util.Random
import scala.util.control.Breaks._
import cspom.Loggable
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMTrue
import cspom.compiler.Delta
import cspom.variable.CSPOMSeq
import scala.collection.mutable.WeakHashMap
import cspom.variable.CSPOMExpression
import cspom.compiler.ConstraintCompilerNoData
import concrete.CSPOMDriver
import CSPOMDriver._
import cspom.variable.BoolVariable

object SetIn extends ConstraintCompilerNoData {

  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) =
    constraint.function == 'set_in && constraint.result == CSPOMTrue

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    val Seq(variable, CSPOMSeq(set, _, _)) = constraint.arguments

    val constraints = for (v <- set) yield {
      new CSPOMConstraint(new BoolVariable(), 'eq, Seq(variable, v))
    }

    val disjunction = CSPOMConstraint('or, constraints.map(_.result))

    replaceCtr(constraint, disjunction +: constraints, problem)

  }
  def selfPropagation = false
}
