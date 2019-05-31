package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.{CSPOM, CSPOMConstraint}
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}
import cspom.util.ContiguousIntRangeSet
import cspom.variable.{BoolVariable, CSPOMConstant, CSPOMSeq, IntExpression}

object NValues extends ConstraintCompilerNoData {

  override def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val domain = constraint.arguments.map {
      case IntExpression(e) => IntExpression.implicits.ranges(e)
    }
      .reduce(_ ++ _)

    var newConstraints = List[CSPOMConstraint[_]]()

    val IntExpression(result) = constraint.result

    val seq = CSPOMSeq(constraint.arguments: _*)

    val occurrences: Seq[BoolVariable] = for (v <- new ContiguousIntRangeSet(domain).toSeq) yield {
      val aux = new BoolVariable()
      newConstraints ::= CSPOMConstraint(aux)('member)(seq, CSPOMConstant(v))
      aux
    }

    newConstraints ::= CSPOMDriver.linear(
      result +: occurrences,
      -1 +: Seq.fill(seq.size)(1),
      "eq",
      0)


    ConstraintCompiler.replaceCtr(constraint, newConstraints, problem)
  }

  def functions = Functions('nvalues)

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = true
}
