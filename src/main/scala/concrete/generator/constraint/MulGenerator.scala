package concrete.generator.constraint;

import Generator.cspom2concrete1D
import concrete.constraint.semantic.ConstProdAC
import concrete.constraint.semantic.ConstProdBC
import concrete.constraint.semantic.MulAC
import concrete.constraint.semantic.MulBC
import cspom.CSPOMConstraint

final object MulGenerator extends Generator {

  override def genFunctional(constraint: CSPOMConstraint[_], r: C2Conc)(implicit variables: VarMap) = {
    val result = r.asInstanceOf[C21D]
    val Seq(v0, v1) = constraint.arguments map cspom2concrete1D

    (result, v0, v1) match {

      case (Const(r: Int), Var(v0), Var(v1)) =>
        val bc = Seq(new ConstProdBC(v0, v1, r))
        if (v0.initDomain.size + v1.initDomain.size < 1000) {
          new ConstProdAC(v0, v1, r) +: bc
        } else {
          bc
        }
      //      case (Const(r), Const(v0), Const(v1)) =>
      //        if (r == v0 * v1) Nil else throw UNSATObject

      // Handled in CSPOM pattern
      //      case (Var(r), Const(v0), Var(v1)) => Seq(
      //        new Sum(0, Array(-1, v0), Array(r, v1), SumMode.SumEQ))
      //      case (Var(r), Var(v0), Const(v1)) => Seq(
      //        new Sum(0, Array(-1, v1), Array(r, v0), SumMode.SumEQ))

      case (Var(r), Var(v0), Var(v1)) => {
        val bc = Seq(new MulBC(r, v0, v1))
        if (r.initDomain.size + v0.initDomain.size + v1.initDomain.size < 1000) {
          new MulAC(r, v0, v1, true) +: bc
        } else {
          bc
        }
      }

      case _ => throw new AssertionError("Unary or binary multiplication is not supported")
    }

  }

}
