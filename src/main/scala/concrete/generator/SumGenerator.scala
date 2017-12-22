package concrete
package generator

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.linear._
import concrete.constraint.semantic.{Neq, NeqC}
import concrete.constraint.{Constraint, ReifiedConstraint}
import concrete.generator.Generator.cspom2concrete1D
import cspom.CSPOMConstraint
import cspom.variable.{CSPOMConstant, IntExpression, SimpleExpression}

object SumGenerator {
  def readCSPOM(constraint: CSPOMConstraint[_]): (IndexedSeq[SimpleExpression[Any]], Seq[Int], Int, SumMode) = {
    require(constraint.arguments.size == 3)
    val IntExpression.constSeq(coefs) = constraint.arguments(0)
    val SimpleExpression.simpleSeq(vars) = constraint.arguments(1)
    val CSPOMConstant(c) = constraint.arguments(2) //map cspom2concreteVar

    // For bool2int optimization
    val constant = util.Math.any2Int(c)

    val mode = constraint.getParam[Any]("mode")
      .flatMap {
        case m: SumMode => Some(m)
        case m: String => SumMode.withName(m) //.getOrElse(throw new IllegalArgumentException("Unknown mode " + m))
      }
      .getOrElse(throw new IllegalArgumentException("Constraint " + constraint + " has no valid mode"))

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

final class SumGenerator(pg: ProblemGenerator) extends Generator with LazyLogging {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)
    val solverVariables = vars.map(cspom2concrete1D).map(_.asVariable(pg))

    withBinSpec(solverVariables, varParams, constant, mode).toSeq
  }

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap) = {
    val r = result.asVariable(pg)

    val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)
    val solverVariables = vars.map(cspom2concrete1D).map(_.asVariable(pg))
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

  def withBinSpec(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode): ACBC =
    solverVariables.size match {
      case 1 =>
        val Seq(x) = solverVariables
        (varParams, mode, constant) match {
          case (Seq(1), SumLE, k) => ACBC.withBC(new LeC(x, k))
          case (Seq(1), SumLT, k) => ACBC.withBC(new LtC(x, k))
          case (Seq(-1), SumLE, k) => ACBC.withBC(new GeC(x, -k))
          case (Seq(-1), SumLT, k) => ACBC.withBC(new GtC(x, -k))
          case (Seq(1), SumNE, k) => ACBC.withBC(new NeqC(x, k))
          case _ =>

            val c = general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode)
            logger.info(s"${c} is non-specialized unary linear constraint")
            c
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
          case (Seq(1, -1), SumEQ, k) => Eq(neg = false, x, -k, y)
          case (Seq(-1, -1), SumEQ, k) => Eq(neg = true, x, -k, y)
          case (Seq(-1, 1), SumEQ, k) => Eq(neg = false, x, k, y)
          case (Seq(1, 1), SumEQ, k) => Eq(neg = true, x, k, y)
          case _ =>
            logger.info(s"${(varParams, mode, constant)} is non-specialized binary linear constraint")
            general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode)
        }

      case _ =>
        general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode)

    }

  def general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode): ACBC = {
    ACBC.withBC(
      Linear(constant, varParams.toArray, solverVariables.toArray, mode, pm))
  }

  def pm = pg.pm

  def reverse(varParams: Seq[Int], constant: Int, mode: SumMode) = {
    mode match {
      case SumEQ => (varParams, constant, SumNE)
      case SumNE => (varParams, constant, SumEQ)
      case SumLT => (varParams.map(-_), -constant, SumLE)
      case SumLE => (varParams.map(-_), -constant, SumLT)
    }
  }
}
