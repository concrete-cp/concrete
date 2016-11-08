package concrete.constraint.semantic

import java.math.BigInteger

import scala.collection.mutable.ArrayBuffer

import org.sat4j.core.Vec
import org.sat4j.core.VecInt
import org.sat4j.specs.ContradictionException

import com.typesafe.scalalogging.LazyLogging

import concrete.BooleanDomain
import concrete.BooleanDomain.FALSE
import concrete.BooleanDomain.TRUE
import concrete.Contradiction
import concrete.Domain
import concrete.Event
import concrete.Outcome
import concrete.ParameterManager
import concrete.ProblemState
import concrete.Variable
import concrete.cluster.Arc
import concrete.cluster.ConnectedComponents
import concrete.constraint.Constraint
import concrete.constraint.linear.Linear
import concrete.constraint.linear.SumEQ
import concrete.constraint.linear.SumLE
import concrete.constraint.linear.SumMode

case class Clause(positive: Seq[Variable], negative: Seq[Variable]) extends Arc {
  require(vars.forall(v => v.initDomain.isInstanceOf[BooleanDomain]), s"some of ${vars.map(v => s"$v ${v.initDomain}")} are not boolean")
  def size = positive.size + negative.size
  def vars = positive ++ negative

  override def toString = s"Clause(${positive.mkString(", ")}${if (positive.nonEmpty && negative.nonEmpty) ", " else ""}${negative.map("-" + _).mkString(", ")})"
}

case class PseudoBoolean(
  vars: Seq[Variable], factors: Seq[Int],
  mode: SumMode, constant: Int)
    extends Arc {

  def size = vars.size

}

object SAT extends LazyLogging {

  def apply(clauses: Seq[Clause] = Seq.empty, pb: Seq[PseudoBoolean] = Seq.empty, pm: ParameterManager): Seq[Constraint] = {
    logger.info("Computing SAT components")

    val comp = ConnectedComponents(clauses ++ pb)

    comp.flatMap { arcs =>
      val c = arcs.collect { case c: Clause => c }
      val p = arcs.collect { case pb: PseudoBoolean => pb }

      assert(c.size + p.size == arcs.size)

      (c, p) match {
        case (Seq(singleClause), Seq()) => Seq(new ClauseConstraint(singleClause))
        case (Seq(), Seq(pb)) => Seq(Linear(pb.constant, pb.factors.toArray, pb.vars.toArray, pb.mode, pm))
        case (clauses, pb) =>
          new SAT((clauses.flatMap(_.vars) ++ pb.flatMap(_.vars)).distinct.toArray, clauses, pb) +:
            clauses.map(new ClauseConstraint(_))
      }
    }

  }

}

class SAT(vars: Array[Variable], clauses: Seq[Clause], pseudo: Seq[PseudoBoolean]) extends Constraint(vars) with LazyLogging {
  private var solver: org.sat4j.specs.ISolverService with org.sat4j.specs.ISolver = _

  logger.info(s"SAT constraint with ${vars.size} vars, ${clauses.size} clauses and ${pseudo.size} pseudo-boolean constraints")

  //solver.setTimeout(0)

  override def init(ps: ProblemState): Outcome = {

    try {
      if (pseudo.isEmpty) {
        solver = org.sat4j.minisat.SolverFactory.newMiniLearningHeap()
        solver.newVar(vars.size)
        solver.setExpectedNumberOfClauses(clauses.size)

        //println(toString(ps))
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

      solver.setTimeoutOnConflicts(Int.MaxValue)
      solver.setKeepSolverHot(true)

      for (c <- clauses) {
        val satClause = new ArrayBuffer[Int](c.size)
        satClause ++= c.positive.map(v => position(v).head + 1)
        satClause ++= c.negative.map(v => -position(v).head - 1)
        solver.addClause(new VecInt(satClause.toArray))
      }

      for ((v, i) <- vars.zipWithIndex) {
        ps.dom(v) match {
          case TRUE => solver.addClause(new VecInt(Array(i + 1)))
          case FALSE => solver.addClause(new VecInt(Array(-i - 1)))
          case _ =>
        }
      }

      ps
    } catch {
      case _: ContradictionException => Contradiction
    }

  }

  def advise(ps: ProblemState, event: Event, pos: Int): Int = if (vars.size > 30) Int.MaxValue else 0x1 << vars.size

  def check(tuple: Array[Int]): Boolean = {
    clauses.forall { c =>
      c.positive.exists(v => tuple(position(v).head) == 1) || c.negative.exists(v => tuple(position(v).head) == 0)
    }
  }

  def revise(ps: ProblemState): Outcome = {
    val state = new VecInt()

    for (i <- 0 until arity) {
      ps.dom(scope(i)) match {
        case TRUE => state.push(i + 1)
        case FALSE => state.push(-i - 1)
        case _ => assert(ps.dom(scope(i)).size > 1)
      }
    }

    //solver.setSearchListener(new TextOutputTracing(null))
    //println(state)

    val model = solver.findModel(state)

    //println(Option(model).toSeq.flatMap(_.toSeq))

    if (model eq null) Contradiction else {
      //      ps.fold(solver.getLiteralsPropagatedAt(0)) { (ps, p) =>
      //        println(p)
      //        ps
      //      }
      ps
      //      ps.fold(solver.getLearnedConstraints.iterator.asScala.toTraversable) { (ps, p) =>
      //        println(p)
      //        ps
      //      }
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
          case TRUE => state.push(i + 1)
          case FALSE => state.push(-i - 1)
          case _ => assert(ps.dom(scope(i)).size > 1)
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

  def findSupport(doms: Array[Domain], position: Int, value: Int) = ???

  def simpleEvaluation: Int = 7
}