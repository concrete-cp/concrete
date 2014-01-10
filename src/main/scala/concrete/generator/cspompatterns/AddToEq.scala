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
import cspom.variable.BoolExpression
import cspom.variable.CSPOMFalse
import cspom.variable.IntExpression
import cspom.variable.IntConstant

/**
 * Addition with constants are converted to equality constraints
 */
object AddToEq extends ConstraintCompiler {

  sealed trait ConstantFound
  case class ConstantResult(r: Int) extends ConstantFound
  case class ConstantArg(a: Int, b: CSPOMVariable) extends ConstantFound
  case class OneVariable(v: CSPOMVariable, r: Int) extends ConstantFound
  case object NoVariable extends ConstantFound

  type A = ConstantFound

  override def matchConstraint(constraint: CSPOMConstraint) = Some(constraint).collect {
    case CSPOMConstraint(a, 'add, Seq(b, c), params) => (a, b, c)
  } collect {
    case (a: IntConstant, b: CSPOMVariable, c: CSPOMVariable) => ConstantResult(a.value)
    case (a: CSPOMVariable, b: IntConstant, c: CSPOMVariable) => ConstantArg(b.value, c)
    case (a: CSPOMVariable, b: CSPOMVariable, c: IntConstant) => ConstantArg(c.value, b)

    case (a: CSPOMVariable, b: IntConstant, c: IntConstant) => OneVariable(a, b.value + c.value)
    case (a: IntConstant, b: CSPOMVariable, c: IntConstant) => OneVariable(b, a.value - c.value)
    case (a: IntConstant, b: IntConstant, c: CSPOMVariable) => OneVariable(c, a.value - b.value)

    case (a: IntConstant, b: IntConstant, c: IntConstant) =>
      require(a.value == b.value + c.value, s"Inconsistent addition: $a = $b + $c")
      NoVariable

  }

  def compile(fc: CSPOMConstraint, problem: CSPOM, d: ConstantFound) = {

    problem.removeConstraint(fc)

    val dr = Delta().removed(fc)

    val added: Option[CSPOMConstraint] = d match {
      case ConstantResult(r: Int) => Some(new CSPOMConstraint(CSPOMTrue, 'eq, fc.arguments, Map("neg" -> true, "offset" -> r)))
      case ConstantArg(a: Int, b: CSPOMVariable) => Some(new CSPOMConstraint(CSPOMTrue, 'eq, Seq(b, fc.result), Map("offset" -> a)))
      case OneVariable(v, r) => Some(new CSPOMConstraint(CSPOMTrue, 'eq, Seq(v, problem.constant(r))))
      case NoVariable => None
    }

    added match {
      case Some(c) => dr.added(problem.ctr(c))
      case None => dr
    }

  }

}
