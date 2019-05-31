package concrete
package generator

import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.linear.SumMode._
import concrete.constraint.linear._
import concrete.constraint.semantic.{Neq, NeqC}
import concrete.constraint.{Constraint, ReifiedConstraint}
import concrete.generator.Generator.cspom2concrete1D
import cspom.CSPOMConstraint
import cspom.variable.{CSPOMConstant, SimpleExpression}

object SumGenerator {
  def readCSPOM(constraint: CSPOMConstraint[_]): (IndexedSeq[SimpleExpression[Any]], Seq[Int], Int, SumMode) = {
    require(constraint.arguments.lengthCompare(3) == 0)
    try {
      val CSPOMConstant.seq(anyCoefs) = constraint.arguments(0)
      val coefs = anyCoefs.map(util.Math.any2Int)
      val SimpleExpression.simpleSeq(vars) = constraint.arguments(1)
      val CSPOMConstant(c) = constraint.arguments(2) //map cspom2concreteVar

      // For bool2int optimization
      val constant = util.Math.any2Int(c)

      val mode = readMode(constraint)
        .getOrElse(throw new IllegalArgumentException("Constraint " + constraint + " has no valid mode"))

      (vars, coefs, constant, mode)
    } catch {
      case e: MatchError =>
        throw new IllegalArgumentException(s"Invalid arguments of $constraint", e)
    }
  }

  def readMode(constraint: CSPOMConstraint[_]): Option[SumMode] = {
    constraint.getParam[Any]("mode")
      .flatMap {
        case m: SumMode => Some(m)
        case m: String => SumMode.withName(m) //.getOrElse(throw new IllegalArgumentException("Unknown mode " + m))
      }
  }
}

object ACBC {
  private val empty: ACBC = ACBC(Seq(), Seq())

  def withAC(c: Constraint*): ACBC = empty.withAC(c: _*)

  def withBC(c: Constraint*): ACBC = empty.withBC(c: _*)
}

case class ACBC(ac: Seq[Constraint], bc: Seq[Constraint]) {
  def toSeq: Seq[Constraint] = ac ++ bc

  def withAC(c: Constraint*): ACBC = {
    ACBC(ac ++ c, bc)
  }

  def withBC(c: Constraint*): ACBC = {
    ACBC(ac, bc ++ c)
  }

  def foreach[U](f: Constraint => U): Unit = {
    ac.foreach(f)
    bc.foreach(f)
  }
}

final class SumGenerator(pg: ProblemGenerator) extends Generator with LazyLogging {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap): Seq[Constraint] = {
    val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)
    val solverVariables = vars.map(cspom2concrete1D).map(_.asVariable(pg))

    withBinSpec(solverVariables, varParams, constant, mode).toSeq
  }

  override def genFunctional(constraint: CSPOMConstraint[_], result: C2Conc)(implicit variables: VarMap): Seq[Constraint] = {
    val r = result.asVariable(pg)

    val (vars, varParams, constant, mode) = SumGenerator.readCSPOM(constraint)
    val solverVariables = vars.map(cspom2concrete1D).map(_.asVariable(pg))
    val positive = withBinSpec(solverVariables, varParams, constant, mode)
    val (negParams, negConstant, negMode) = reverse(varParams, constant, mode)
    val negative = withBinSpec(solverVariables, negParams, negConstant, negMode)

    val ac = positive.ac.map { pac => new ReifiedConstraint(neg = false, r, pac) } ++
      negative.ac.map { nac => new ReifiedConstraint(neg = true, r, nac) }

    val bc = positive.bc.map { pbc => new ReifiedConstraint(neg = false, r, pbc) } ++
      negative.bc.map { nbc => new ReifiedConstraint(neg = true, r, nbc) }

    val reified = ACBC(ac, bc).toSeq

    require(reified.nonEmpty, s"$positive resulted in no reified constraints")
    reified

  }

  def withBinSpec(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode): ACBC =
    solverVariables.size match {
      case 1 =>
        val Seq(x) = solverVariables
        (varParams, mode, constant) match {
          case (Seq(1), LE, k) => ACBC.withBC(new LeC(x, k))
          case (Seq(1), LT, k) => ACBC.withBC(new LtC(x, k))
          case (Seq(-1), LE, k) => ACBC.withBC(new GeC(x, -k))
          case (Seq(-1), LT, k) => ACBC.withBC(new GtC(x, -k))
          case (Seq(1), NE, k) => ACBC.withBC(new NeqC(x, k))
          case _ =>
            val c = general(solverVariables: Seq[Variable], varParams: Seq[Int], constant: Int, mode: SumMode)
            logger.info(s"$c is non-specialized unary linear constraint")
            c
        }
      case 2 =>
        val Seq(x, y) = solverVariables
        (varParams, mode, constant) match {
          case (Seq(1, -1), LE, k) => ACBC.withBC(new Gt(y, k, x, false))
          case (Seq(1, -1), LT, k) => ACBC.withBC(new Gt(y, k, x, true))
          case (Seq(-1, 1), LE, k) => ACBC.withBC(new Gt(x, k, y, false))
          case (Seq(-1, 1), LT, k) => ACBC.withBC(new Gt(x, k, y, true))
          case (Seq(1, -1), NE, k) => ACBC.withAC(new Neq(x, y, k))
          case (Seq(-1, 1), NE, k) => ACBC.withAC(new Neq(x, y, -k))
          case (Seq(1, -1), EQ, k) => Eq(neg = false, x, -k, y)
          case (Seq(-1, -1), EQ, k) => Eq(neg = true, x, -k, y)
          case (Seq(-1, 1), EQ, k) => Eq(neg = false, x, k, y)
          case (Seq(1, 1), EQ, k) => Eq(neg = true, x, k, y)
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

  def pm: ParameterManager = pg.pm

  def reverse(varParams: Seq[Int], constant: Int, mode: SumMode): (Seq[Int], Int, SumMode) = {
    mode match {
      case EQ => (varParams, constant, NE)
      case NE => (varParams, constant, EQ)
      case LT => (varParams.map(-_), -constant, LE)
      case LE => (varParams.map(-_), -constant, LT)
      case e => throw new IllegalArgumentException(s"$e not supported, convert to LE/LT first")
    }
  }
}
