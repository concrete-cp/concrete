package concrete.generator.constraint;

import concrete.constraint.semantic.Bounds
import concrete.constraint.semantic.Gcc
import concrete.{ Variable, Problem }
import cspom.{ CSPOMConstraint }
import concrete.constraint.semantic.OccurrenceVar
//import concrete.constraint.semantic.OccurrenceConst
import Generator._
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq

final object OccurrenceGenerator extends Generator {
  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {

    val Seq(CSPOMConstant(anyVal), CSPOMSeq(vars)) = constraint.arguments

    val value = anyVal match {
      case false  => 0
      case true   => 1
      case v: Int => v
      case a      => throw new IllegalArgumentException(s"Counting $a is not supported")
    }

    val args = vars.map(cspom2concrete1D)

    val scope = args.collect {
      case Var(v) => v
    }

    val constOcc = args.collect {
      case Const(c) => c
    } count {
      _ == value
    }

    Seq(new OccurrenceVar(r.asVariable, value, scope.toArray, constOcc))
//    r match {
//      case Const(result) => 
//        Seq(new OccurrenceConst(result - constOcc, value, scope.toArray))
//      case Var(result) =>
//        Seq(new OccurrenceVar(result, value, scope.toArray, constOcc))
//      case _ => throw new IllegalArgumentException(s"Result must be a variable or constant, was $r")
//    }

  }
}
