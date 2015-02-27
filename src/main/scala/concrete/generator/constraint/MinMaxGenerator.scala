package concrete.generator.constraint;

import Generator._
import concrete.Variable
import concrete.constraint.semantic.Max
import concrete.constraint.semantic.Min
import cspom.CSPOMConstraint

final object MinGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val args = constraint.arguments map cspom2concrete1D

    val vars = args.collect {
      case Var(v) => v
    } ++
      args.collect {
        case Const(c) => c
      }
      .reduceOption(math.min)
      .map(c => Const(c).asVariable)

    Seq(new Min(r.asVariable, vars.toArray))
  }

}
final object MaxGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val args = constraint.arguments map cspom2concrete1D

    val vars = args.collect {
      case Var(v) => v
    } ++
      args.collect {
        case Const(c) => c
      }
      .reduceOption(math.max)
      .map(c => Const(c).asVariable)

    Seq(new Max(r.asVariable, vars.toArray))
  }

}

