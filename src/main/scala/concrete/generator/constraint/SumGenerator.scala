package concrete.generator.constraint;

import scala.reflect.runtime.universe

import com.typesafe.scalalogging.LazyLogging

import Generator.cspom2concreteVar
import concrete.constraint.semantic.Eq
import concrete.constraint.semantic.Gt
import concrete.constraint.semantic.Neq
import concrete.constraint.semantic.SumAC
import concrete.constraint.semantic.SumBC
import concrete.constraint.semantic.SumMode
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq

final object SumGenerator extends Generator with LazyLogging {

  def readCSPOM(constraint: CSPOMConstraint[_])(implicit variables: VarMap) = {
    val Seq(CSPOMSeq(vars), CSPOMConstant(c)) = constraint.arguments //map cspom2concreteVar

    // For bool2int optimization
    val constant = c match {
      case i: Int => i
      case false  => 0
      case true   => 1
    }

    val params = constraint.params.get("coefficients") match {
      case Some(p: Seq[_]) => p.asInstanceOf[Seq[Int]]
      case None            => Seq.fill(vars.length)(1)
      case _               => throw new IllegalArgumentException("Parameters for zero sum must be a sequence of integer values")
    }

    val solverVariables = vars.map(cspom2concreteVar)

    val mode = constraint.params.get("mode").collect {
      case m: String => SumMode.withName(m)
    }.get

    (solverVariables, params, constant, mode)

  }

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {

    val (solverVariables, varParams, constant, mode) = readCSPOM(constraint)

    if (solverVariables.size == 2) {
      val Seq(x, y) = solverVariables
      (varParams, mode, constant) match {
        case (Seq(1, -1), SumMode.SumLE, k) => Seq(new Gt(y, k, x, false))
        case (Seq(1, -1), SumMode.SumLT, k) => Seq(new Gt(y, k, x, true))
        case (Seq(-1, 1), SumMode.SumLE, k) => Seq(new Gt(x, k, y, false))
        case (Seq(-1, 1), SumMode.SumLT, k) => Seq(new Gt(x, k, y, true))
        case (Seq(-1, 1) | Seq(1, -1), SumMode.SumNE, 0) =>
          throw new AssertionError("Not-Equal linear constraint should be captured by SumNe CSPOM pattern")
        case (Seq(1, -1), SumMode.SumEQ, k)  => Eq(false, x, -k, y)
        case (Seq(-1, -1), SumMode.SumEQ, k) => Eq(true, x, -k, y)
        case (Seq(-1, 1), SumMode.SumEQ, k)  => Eq(false, x, k, y)
        case (Seq(1, 1), SumMode.SumEQ, k)   => Eq(true, x, k, y)
        case _ =>
          logger.warn(s"$constraint is non-specialized binary linear constraint")
          Seq(new SumBC(constant, varParams.toArray, solverVariables.toArray, mode))
      }

    } else {
      val bc = new SumBC(constant, varParams.toArray, solverVariables.toArray, mode)

      val ss = CSPOMSeq(constraint.arguments: _*).searchSpace
      //println(ss)

      if (ss < 10000) {
        Seq(bc, new SumAC(constant, varParams.toArray, solverVariables.toArray, mode))
      } else {
        Seq(bc)
      }
    }
  }
}
