package concrete.generator.constraint;

import Generator._
import concrete.Variable
import concrete.constraint.semantic.Max
import concrete.constraint.semantic.Min
import cspom.CSPOMConstraint

final object MinGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val Var(result) = r
    val args = constraint.arguments map cspom2concrete1D

    val vars = args.collect {
      case Var(v) => v
    }

    val minConst = args
      .collect {
        case Const(c) => c
      }
      .reduceOption(math.min)

    Seq(new Min(result, vars.toArray, minConst))
  }

}
final object MaxGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val Var(result) = r
    val args = constraint.arguments map cspom2concrete1D

    val vars = args.collect {
      case Var(v) => v
    }

    val minConst = args
      .collect {
        case Const(c) => c
      }
      .reduceOption(math.max)

    Seq(new Max(result, vars.toArray, minConst))
  }

}

