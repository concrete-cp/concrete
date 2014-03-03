package concrete.generator.cspompatterns

import cspom.CSPOM
import scala.collection.mutable.Queue
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.BoolVariable
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMConstant
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMFalse

/**
 * Addition with constants are converted to equality constraints
 */
object AddToEq extends ConstraintCompiler {

  sealed trait ConstantFound
  case class ConstantResult(r: Int) extends ConstantFound
  case class ConstantArg(a: Int, b: CSPOMVariable[_]) extends ConstantFound
  case class OneVariable(v: CSPOMVariable[_], r: Int) extends ConstantFound
  case object NoVariable extends ConstantFound

  type A = ConstantFound

  override def matchConstraint(constraint: CSPOMConstraint[_]) = Some(constraint).collect {
    case CSPOMConstraint(a, 'add, Seq(b, c), params) => (a, b, c)
  } collect {
    case (CSPOMConstant(a: Int), b: CSPOMVariable[_], c: CSPOMVariable[_]) => ConstantResult(a)
    case (a: CSPOMVariable[_], CSPOMConstant(b: Int), c: CSPOMVariable[_]) => ConstantArg(b, c)
    case (a: CSPOMVariable[_], b: CSPOMVariable[_], CSPOMConstant(c: Int)) => ConstantArg(c, b)

    case (a: CSPOMVariable[_], CSPOMConstant(b: Int), CSPOMConstant(c: Int)) => OneVariable(a, b + c)
    case (CSPOMConstant(a: Int), b: CSPOMVariable[_], CSPOMConstant(c: Int)) => OneVariable(b, a - c)
    case (CSPOMConstant(a: Int), CSPOMConstant(b: Int), c: CSPOMVariable[_]) => OneVariable(c, a - b)

    case (CSPOMConstant(a: Int), CSPOMConstant(b: Int), CSPOMConstant(c: Int)) =>
      require(a == b + c, s"Inconsistent addition: $a = $b + $c")
      NoVariable

  }

  def compile(fc: CSPOMConstraint[_], problem: CSPOM, d: ConstantFound) = {

    val added = d match {
      case ConstantResult(r) => Seq(CSPOMConstraint('eq, fc.arguments, Map("neg" -> true, "offset" -> r)))
      case ConstantArg(a, b) => Seq(CSPOMConstraint('eq, Seq(b, fc.result), Map("offset" -> a)))
      case OneVariable(v, r) => Seq(CSPOMConstraint('eq, Seq(v, CSPOMConstant(r))))
      case NoVariable => Nil
    }

    replaceCtr(fc, added, problem)

  }

  def selfPropagation = false
}
