package concrete.generator;

import com.typesafe.scalalogging.LazyLogging

import Generator.cspom2concrete1D
import concrete.ParameterManager
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.ReifiedConstraint
import concrete.constraint.linear.Eq
import concrete.constraint.linear.Gt
import concrete.constraint.linear.LeC
import concrete.constraint.linear.Linear
import concrete.constraint.linear.LtC
import concrete.constraint.linear.SumEQ
import concrete.constraint.linear.SumLE
import concrete.constraint.linear.SumLT
import concrete.constraint.linear.SumMode
import concrete.constraint.linear.SumNE
import concrete.constraint.semantic.Neq
import cspom.CSPOMConstraint
import cspom.variable.CSPOMConstant
import cspom.variable.IntExpression
import cspom.variable.SimpleExpression
import concrete.constraint.linear.GeC
import concrete.constraint.linear.GtC

object SumGenerator {
  def readCSPOM(constraint: CSPOMConstraint[_]) = {
    val Seq(IntExpression.constSeq(coefs), SimpleExpression.simpleSeq(vars), CSPOMConstant(c)) = constraint.arguments //map cspom2concreteVar

    // For bool2int optimization
    val constant = c match {
      case i: Int => i
      case false => 0
      case true => 1
    }

    val mode = constraint.getParam[String]("mode")
      .flatMap(SumMode.withName)
      .get

    (vars, coefs, constant, mode)

  }
}

object ACBC {
  private val empty: ACBC = ACBC(None, None)
  def withAC(c: Constraint) = empty.withAC(c)
  def withBC(c: Constraint) = empty.withBC(c)
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

  def foreach[U](f: Constraint => U) = {
    ac.foreach(f)
    bc.foreach(f)
  }

}

final class SumGenerator(pm: ParameterManager) extends Generator with LazyLogging {

  def eq(neg: Boolean, x: Variable, b: Int, y: Variable): ACBC =
    Eq(neg, x, b, y)

  def general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode): ACBC = {
    val constraints = ACBC.withBC(
      Linear(constant, varParams.toArray, solverVariables.toArray, mode, pm))

    //   lazy val ss = Domain.searchSpace(solverVariables.map(_.initDomain))
    //println(ss)

    //    if ((mode == SumEQ || mode == SumNE)) { //} && ss < 0) {
    //      constraints.withAC(
    //        new SumAC(constant, varParams.toArray, solverVariables.toArray, mode))
    //    } else {
    constraints
    //    }

  }

  def withBinSpec(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode) =
    solverVariables.size match {
      case 1 =>
        val Seq(x) = solverVariables
        (varParams, mode, constant) match {
          case (Seq(1), SumLE, k) => ACBC.withBC(new LeC(x, k))
          case (Seq(1), SumLT, k) => ACBC.withBC(new LtC(x, k))
          case (Seq(-1), SumLE, k) => ACBC.withBC(new GeC(x, -k))
          case (Seq(-1), SumLT, k) => ACBC.withBC(new GtC(x, -k))
          case _ =>
            logger.info(s"${(varParams, mode, constant)} is non-specialized unary linear constraint")
            general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode)
        }
      case 2 =>
        val Seq(x, y) = solverVariables
        (varParams, mode, constant) match {
          case (Seq(1, -1), SumLE, k) => ACBC.withBC(new Gt(y, k, x, false))
          case (Seq(1, -1), SumLT, k) => ACBC.withBC(new Gt(y, k, x, true))
          case (Seq(-1, 1), SumLE, k) => ACBC.withBC(new Gt(x, k, y, false))
          case (Seq(-1, 1), SumLT, k) => ACBC.withBC(new Gt(x, k, y, true))
          case (Seq(1, -1), SumNE, k) => ACBC.withAC(new Neq(x, y, k))
          case (Seq(-1, 1), SumNE, k) => ACBC.withAC(new Neq(x, y, -k))
          case (Seq(1, -1), SumEQ, k) => eq(false, x, -k, y)
          case (Seq(-1, -1), SumEQ, k) => eq(true, x, -k, y)
          case (Seq(-1, 1), SumEQ, k) => eq(false, x, k, y)
          case (Seq(1, 1), SumEQ, k) => eq(true, x, k, y)
          case _ =>
            logger.info(s"${(varParams, mode, constant)} is non-specialized binary linear constraint")
            general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode)
        }

      case _ =>
        general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode)

    }

  def reverse(varParams: Seq[Int], constant: Int, mode: SumMode) = {
    mode match {
      case SumEQ => (varParams, constant, SumNE)
      case SumNE => (varParams, constant, SumEQ)
      case SumLT => (varParams.map(-_), -constant, SumLE)
      case SumLE => (varParams.map(-_), -constant, SumLT)
    }
  }

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)
    val solverVariables = vars.map(cspom2concrete1D).map(_.asVariable)

    withBinSpec(solverVariables, varParams, constant, mode).toSeq
  }

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val r = result.asVariable

    val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)
    val solverVariables = vars.map(cspom2concrete1D).map(_.asVariable)
    val positive = withBinSpec(solverVariables, varParams, constant, mode)
    val (negParams, negConstant, negMode) = reverse(varParams, constant, mode)
    val negative = withBinSpec(solverVariables, negParams, negConstant, negMode)

    val ac = positive.ac
      .map { pac =>
        val neg = negative.ac.orElse(negative.bc).get
        new ReifiedConstraint(r, pac, neg)
      }
      .orElse {
        for (
          pbc <- positive.bc;
          nac <- negative.ac
        ) yield new ReifiedConstraint(r, pbc, nac)
      }

    val bc = positive.bc
      .map {
        pbc =>
          val neg = negative.bc.orElse(negative.ac).get
          new ReifiedConstraint(r, pbc, neg)
      }
      .orElse {
        for (pac <- positive.ac; nbc <- negative.bc) yield new ReifiedConstraint(r, pac, nbc)
      }

    val reified = ACBC(ac, bc).toSeq

    require(reified.nonEmpty, s"$positive resulted in no reified constraints")
    reified

  }
}
