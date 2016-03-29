package concrete.generator;

import Generator.cspom2concrete1D
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
        case Const(c: Int) => c
        case o @ Const(_)  => throw new IllegalArgumentException(s"$o has not a valid type for min constraint")
      }
      .reduceOption(math.min)
      .map(c => Const(c).asVariable)

    Seq(Min(r.asVariable, vars))
  }

}
final object MaxGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val args = constraint.arguments map cspom2concrete1D

    val vars = args.collect {
      case Var(v) => v
    } ++
      args.collect {
        case Const(c: Int) => c
        case o @ Const(_)  => throw new IllegalArgumentException(s"$o has not a valid type for min constraint")
      }
      .reduceOption(math.max)
      .map(c => Const(c).asVariable)

    Seq(Max(r.asVariable, vars))
  }

}

