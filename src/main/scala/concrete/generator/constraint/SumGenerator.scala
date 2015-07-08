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
import Generator.cspom2concrete1D
import concrete.constraint.semantic.SumMode._
import concrete.Variable
import concrete.Domain
import concrete.constraint.semantic.ReifiedConstraint
import concrete.constraint.semantic.SumNE
import concrete.constraint.BC
import concrete.constraint.Constraint
import concrete.constraint.semantic.EqAC
import concrete.constraint.semantic.EqBC
import cspom.variable.IntExpression

final object SumGenerator extends Generator with LazyLogging {

  object ACBC {
    val empty: ACBC = ACBC(None, None)
    def apply(): ACBC = empty
  }

  case class ACBC(ac: Option[Constraint], bc: Option[Constraint]) {
    def toSeq = ac.toSeq ++ bc.toSeq
    def withAC(c: Constraint) = {
      require(ac.isEmpty)
      ACBC(Some(c), bc)
    }

    def withBC(c: Constraint) = {
      require(bc.isEmpty)
      ACBC(ac, Some(c))
    }
  }

  def eq(neg: Boolean, x: Variable, b: Int, y: Variable) = ACBC(
    ac = Some(new EqAC(neg, x, b, y)),
    bc = Some(new EqBC(neg, x, b, y)))

  def readCSPOM(constraint: CSPOMConstraint[_]) = {
    val Seq(IntExpression.constSeq(coefs), IntExpression.seq(vars), CSPOMConstant(c)) = constraint.arguments //map cspom2concreteVar

    // For bool2int optimization
    val constant = c match {
      case i: Int => i
      case false  => 0
      case true   => 1
    }

    require(!constraint.params.contains("coefficients"), "coefficients parameter is deprecated")

    val mode = constraint.getParam[String]("mode")
      .map(SumMode.withName)
      .get

    (vars, coefs, constant, mode)

  }

  def general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode.SumMode): ACBC = {
    val constraints = ACBC().withBC(mode match {
      case SumNE => new SumNE(constant, varParams.toArray, solverVariables.toArray)
      case m     => new SumBC(constant, varParams.toArray, solverVariables.toArray, mode)
    })

    val ss = Domain.searchSpace(solverVariables.map(_.initDomain))
    //println(ss)

    if (ss < 10000) {
      constraints.withAC(
        new SumAC(constant, varParams.toArray, solverVariables.toArray, mode))
    } else {
      constraints
    }

  }

  def withBinSpec(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode.SumMode) = {
    if (solverVariables.size == 2) {
      val Seq(x, y) = solverVariables
      (varParams, mode, constant) match {
        case (Seq(1, -1), SumLE, k)  => ACBC().withBC(new Gt(y, k, x, false))
        case (Seq(1, -1), SumLT, k)  => ACBC().withBC(new Gt(y, k, x, true))
        case (Seq(-1, 1), SumLE, k)  => ACBC().withBC(new Gt(x, k, y, false))
        case (Seq(-1, 1), SumLT, k)  => ACBC().withBC(new Gt(x, k, y, true))
        case (Seq(1, -1), SumNE, k)  => ACBC().withAC(new Neq(x, y, k))
        case (Seq(-1, 1), SumNE, k)  => ACBC().withAC(new Neq(x, y, -k))
        case (Seq(1, -1), SumEQ, k)  => eq(false, x, -k, y)
        case (Seq(-1, -1), SumEQ, k) => eq(true, x, -k, y)
        case (Seq(-1, 1), SumEQ, k)  => eq(false, x, k, y)
        case (Seq(1, 1), SumEQ, k)   => eq(true, x, k, y)
        case _ =>
          logger.warn(s"${(varParams, mode, constant)} is non-specialized binary linear constraint")
          general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode.SumMode)
      }

    } else {
      general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode.SumMode)
    }
  }

  def reverse(varParams: Seq[Int], constant: Int, mode: SumMode.SumMode) = {
    mode match {
      case SumEQ => (varParams, constant, SumNE)
      case SumNE => (varParams, constant, SumEQ)
      case SumLT => (varParams.map(-_), -constant, SumLE)
      case SumLE => (varParams.map(-_), -constant, SumLT)
    }
  }

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val (vars, varParams, constant, mode) = readCSPOM(constraint)
    val solverVariables = vars.map(cspom2concrete1D).map(_.asVariable)
    withBinSpec(solverVariables, varParams, constant, mode).toSeq
  }

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val r = result.asVariable

    val (vars, varParams, constant, mode) = readCSPOM(constraint)
    val solverVariables = vars.map(cspom2concrete1D).map(_.asVariable)
    val positive = withBinSpec(solverVariables, varParams, constant, mode)
    val (negParams, negConstant, negMode) = reverse(varParams, constant, mode)
    val negative = withBinSpec(solverVariables, negParams, negConstant, negMode)

    val reified =
      ACBC(
        ac = positive.ac.map {
          pac =>
            val neg = negative.ac.orElse(negative.bc).get
            new ReifiedConstraint(r, pac, neg)
        },
        bc = positive.bc.map {
          pbc =>
            val neg = negative.bc.orElse(negative.ac).get
            new ReifiedConstraint(r, pbc, neg)
        })
        .toSeq

    require(reified.nonEmpty, s"$positive resulted in no reified constraints")
    reified

  }
}
