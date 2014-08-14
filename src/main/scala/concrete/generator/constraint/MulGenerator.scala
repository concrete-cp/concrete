package concrete.generator.constraint;

import concrete.generator.FailedGenerationException
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint
import concrete.UNSATObject
import concrete.constraint.Residues
import concrete.constraint.Constraint
import concrete.constraint.TupleEnumerator
import concrete.constraint.semantic.Sum
import Generator._
import concrete.constraint.semantic.ConstProdAC
import concrete.constraint.semantic.SumMode
import concrete.constraint.semantic.MulAC
import concrete.constraint.semantic.MulBC
import concrete.constraint.semantic.ConstProdBC

final object MulGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val result = r.asInstanceOf[C21D]
    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    (result, v0, v1) match {

      case (Const(r), Var(v0), Var(v1)) =>
        val bc = Seq(new ConstProdBC(v0, v1, r))
        if (v0.dom.size + v1.dom.size < 1000) {
          new ConstProdAC(v0, v1, r) +: bc
        } else {
          bc
        }
      case (Const(r), Const(v0), Const(v1)) =>
        if (r == v0 * v1) Nil else throw UNSATObject

      // Handled in CSPOM pattern
      //      case (Var(r), Const(v0), Var(v1)) => Seq(
      //        new Sum(0, Array(-1, v0), Array(r, v1), SumMode.SumEQ))
      //      case (Var(r), Var(v0), Const(v1)) => Seq(
      //        new Sum(0, Array(-1, v1), Array(r, v0), SumMode.SumEQ))

      case (Var(r), Var(v0), Var(v1)) => {
        val bc = Seq(new MulBC(r, v0, v1))
        if (r.dom.size + v0.dom.size + v1.dom.size < 1000) {
          new MulAC(r, v0, v1, true) +: bc
        } else {
          bc
        }
      }

      case _ => throw new AssertionError("Unary or binary multiplication is not supported")
    }

  }

}
