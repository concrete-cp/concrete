package concrete.generator.cspompatterns
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import scala.util.Random
import scala.util.control.Breaks._
import com.typesafe.scalalogging.LazyLogging
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMConstant
import cspom.compiler.Delta
import cspom.variable.CSPOMSeq
import scala.collection.mutable.WeakHashMap
import cspom.variable.CSPOMExpression
import cspom.compiler.ConstraintCompilerNoData
import concrete.CSPOMDriver
import CSPOMDriver._
import cspom.variable.BoolVariable
import cspom.variable.IntVariable
import cspom.variable.SimpleExpression

object SetIn extends ConstraintCompilerNoData {

  def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) =
    constraint.function == 'set_in && constraint.nonReified

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    val Seq(variable: SimpleExpression[_], CSPOMConstant(set: Seq[Int] @unchecked)) = constraint.arguments

    replaceCtr(constraint, Nil, problem) ++
      replace(Seq(variable), variable.intersected(IntVariable.ofSeq(set: _*)), problem)

  }
  def selfPropagation = false
}
