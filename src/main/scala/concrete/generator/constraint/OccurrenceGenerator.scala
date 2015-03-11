package concrete.generator.constraint;

import concrete.constraint.semantic.Bounds
import concrete.constraint.semantic.Gcc
import concrete.{ Variable, Problem }
import cspom.{ CSPOMConstraint }
import concrete.constraint.semantic.Occurrence
import Generator._
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMExpression
import concrete.constraint.Constraint

final object OccurrenceGenerator extends Generator {
  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val Seq(value: C21D, Sequence(vars, _)) = constraint.arguments.map(cspom2concrete)

    Seq(new Occurrence(r.asVariable, value.asVariable, vars.map(_.asVariable).toArray))
    //    
    //    any match {
    //      case CSPOMConstant(anyVal)    => genWithValue(anyVal, r, vars)
    //
    //      case anyVar: CSPOMVariable[_] => throw new IllegalArgumentException(s"Counting variables is not supported for $constraint")
    //
    //    }

  }
  //
  //  private def genWithValue(anyVal: Any, r: C2Conc, vars: Seq[CSPOMExpression[Any]])(implicit variables: VarMap): Seq[Constraint] = {
  //    val value = anyVal match {
  //      case false  => 0
  //      case true   => 1
  //      case v: Int => v
  //      case a      => throw new IllegalArgumentException(s"Counting $a is not supported")
  //    }
  //    val args = vars.map(cspom2concrete1D)
  //
  //    val scope = args.collect {
  //      case Var(v) => v
  //    }
  //    
  //    require(scope.nonEmpty)
  //
  //    val constOcc = args.collect {
  //      case Const(c) => c
  //    } count {
  //      _ == value
  //    }
  //
  //    Seq(new Occurrence(r.asVariable, value, scope.toArray, constOcc))
  //
  //  }
}
