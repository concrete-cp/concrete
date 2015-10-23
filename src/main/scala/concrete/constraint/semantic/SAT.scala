package concrete.constraint.semantic

import java.math.BigInteger

import scala.collection.mutable.ArrayBuffer

import org.sat4j.core.Vec
import org.sat4j.core.VecInt
import org.sat4j.specs.ContradictionException

import com.typesafe.scalalogging.LazyLogging

import concrete.BooleanDomain
import concrete.Contradiction
import concrete.FALSE
import concrete.Outcome
import concrete.ProblemState
import concrete.TRUE
import concrete.Variable
import concrete.cluster.Arc
import concrete.cluster.ConnectedComponents
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.linear.SumEQ
import concrete.constraint.linear.SumLE
import concrete.constraint.linear.SumMode
import concrete.generator.LinearConstraint

case class Clause(positive: Seq[Variable], negative: Seq[Variable]) extends Arc {
  require(vars.forall(v => v.initDomain.isInstanceOf[BooleanDomain]))
  def size = positive.size + negative.size
  def vars = positive ++ negative
}

class PseudoBoolean(
  vars: Seq[Variable], coefs: Seq[Int],
  mode: SumMode, constant: Int)
    extends LinearConstraint(vars, coefs, mode, constant)

object SAT extends LazyLogging {

  //  def apply(clauses: Seq[Clause], pb: Seq[PseudoBoolean], pm: ParameterManager): Seq[Constraint] = {
  //    val useSAT: Boolean = pm.getOrElse("sat", false)
  //
  //    if (useSAT) {
  //      genSAT(clauses, pb)
  //    } else {
  //      (for (c <- clauses) yield new ClauseConstraint(c)) ++
  //        (for (p <- pb) yield Linear(p.constant, p.coefs.toArray, p.vars.toArray, p.mode, pm))
  //
  //    }
  //  }

  def apply(clauses: Seq[Clause] = Seq.empty, pb: Seq[PseudoBoolean] = Seq.empty): Seq[Constraint] = {
    logger.info("Computing SAT components")

    val comp = ConnectedComponents(clauses ++ pb)

    for (arcs <- comp) yield {
      val c = arcs.collect { case c: Clause => c }
      val p = arcs.collect { case pb: PseudoBoolean => pb }

      assert(c.size + p.size == arcs.size)

      (c, p) match {
        case (Seq(singleClause), Seq()) => new ClauseConstraint(singleClause)
        case (clauses, pb)              => new SAT(clauses.flatMap(_.vars).distinct.toArray, clauses, pb)
      }
    }
  }

}

class SAT(vars: Array[Variable], clauses: Seq[Clause], pseudo: Seq[PseudoBoolean]) extends Constraint(vars) with Residues with LazyLogging {
  private var solver: org.sat4j.specs.ISolverService with org.sat4j.specs.ISolver = _

  logger.info(s"SAT constraint with ${vars.size} vars, ${clauses.size} clauses and ${pseudo.size} pseudo-boolean constraints")

  //solver.setTimeout(0)

  override def init(ps: ProblemState): Outcome = {

    try {
      if (pseudo.isEmpty) {
        solver = org.sat4j.minisat.SolverFactory.newMiniLearningHeap()
        solver.newVar(vars.size)
        solver.setExpectedNumberOfClauses(clauses.size)
      } else {
        val solver = org.sat4j.pb.SolverFactory.newCompetPBResMixedConstraintsObjectiveExpSimp()
        solver.newVar(vars.size)
        solver.setExpectedNumberOfClauses(clauses.size + pseudo.size)
        for (p <- pseudo) {
          p.mode match {
            case SumLE =>
              solver.addPseudoBoolean(
                new VecInt(p.vars.map(v => position(v).head + 1).toArray),
                new Vec(p.factors.map(BigInteger.valueOf(_)).toArray),
                false,
                BigInteger.valueOf(p.constant))
            case SumEQ =>
              solver.addPseudoBoolean(
                new VecInt(p.vars.map(v => position(v).head + 1).toArray),
                new Vec(p.factors.map(BigInteger.valueOf(_)).toArray),
                false,
                BigInteger.valueOf(p.constant))
              solver.addPseudoBoolean(
                new VecInt(p.vars.map(v => position(v).head + 1).toArray),
                new Vec(p.factors.map(BigInteger.valueOf(_)).toArray),
                true,
                BigInteger.valueOf(p.constant))
            case other => throw new IllegalArgumentException(s"$other mode is not supported for SAT PB constraint")

          }

        }
        this.solver = solver
      }

      solver.setKeepSolverHot(true)

      for (c <- clauses) {
        val satClause = new ArrayBuffer[Int](c.size)
        satClause ++= c.positive.map(v => position(v).head + 1)
        satClause ++= c.negative.map(v => -position(v).head - 1)
        solver.addClause(new VecInt(satClause.toArray))
      }

      for ((v, i) <- vars.zipWithIndex) {
        ps.dom(v) match {
          case TRUE  => solver.addClause(new VecInt(Array(i + 1)))
          case FALSE => solver.addClause(new VecInt(Array(-i - 1)))
          case _     =>
        }
      }

      ps
    } catch {
      case _: ContradictionException => Contradiction
    }

  }

  def getEvaluation(problemState: concrete.ProblemState): Int = vars.size * vars.size

  def check(tuple: Array[Int]): Boolean = {
    clauses.forall { c =>
      c.positive.exists(v => tuple(position(v).head) == 1) || c.negative.exists(v => tuple(position(v).head) == 0)
    }
  }
  def findSupport(ps: ProblemState, pos: Int, value: Int): Option[Array[Int]] = {
    val state = new VecInt()

    for (i <- 0 until arity) {
      if (i == pos) {
        if (value == 0) {
          state.push(-i - 1)
        } else {
          state.push(i + 1)
        }
      } else {
        ps.dom(scope(i)) match {
          case TRUE  => state.push(i + 1)
          case FALSE => state.push(-i - 1)
          case _     => assert(ps.dom(scope(i)).size > 1)
        }
      }
    }

    //logger.info(s"SAT with $state")

    val model = solver.findModel(state)

    //    println("SAT run")
    //    println(state)
    //    println(Option(model).map(_.toSeq))
    //    for (c <- JavaConversions.asScalaIterator(solver.getLearnedConstraints.iterator)) {
    //      println(c)
    //    }

    Option(model).map {
      model =>
        val support = new Array[Int](arity)

        for (m <- model) {
          if (m > 0) {
            support(m - 1) = 1
          }
        }

        support

    }
  }

  def simpleEvaluation: Int = 7
}