package concrete.constraint.semantic

import java.math.BigInteger
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import org.sat4j.core.Vec
import org.sat4j.core.VecInt
import org.sat4j.specs.ContradictionException
import org.sat4j.specs.ISolver
import com.typesafe.scalalogging.LazyLogging
import concrete.Contradiction
import concrete.FALSE
import concrete.Outcome
import concrete.ProblemState
import concrete.TRUE
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Residues
import scala.collection.JavaConversions
import concrete.ParameterManager
import concrete.BooleanDomain

case class Clause(positive: Seq[Variable], negative: Seq[Variable]) {
  require(vars.forall(v => v.initDomain.isInstanceOf[BooleanDomain]))
  def size = positive.size + negative.size
  def vars = positive ++ negative
}

case class PseudoBoolean(
    vars: Seq[Variable], coefs: Seq[Int],
    mode: SumMode.SumMode, constant: Int) {
  def size = vars.size
}

object SAT extends LazyLogging {

  def apply(clauses: Seq[Clause], pb: Seq[PseudoBoolean], pm: ParameterManager): Seq[Constraint] = {
    val useSAT: Boolean = pm.getOrElse("sat", false)

    if (useSAT) {
      genSAT(clauses, pb)
    } else {
      (for (c <- clauses) yield new ClauseConstraint(c)) ++
        (for (p <- pb) yield new SumBC(p.constant, p.coefs.toArray, p.vars.toArray, p.mode))

    }
  }

  private def genSAT(clauses: Seq[Clause], pb: Seq[PseudoBoolean]): Seq[Constraint] = {
    logger.info("Computing SAT components")

    var neighbours = new HashMap[Variable, Set[Variable]]().withDefaultValue(Set())
    for (c <- clauses) {
      for (Seq(v1, v2) <- c.vars.combinations(2)) {
        neighbours(v1) += v2
        neighbours(v2) += v1
      }
    }
    for (c <- pb) {
      for (Seq(v1, v2) <- c.vars.combinations(2)) {
        neighbours(v1) += v2
        neighbours(v2) += v1
      }
    }

    val neighb = neighbours.toMap

    var visited: Set[Variable] = Set()
    var components: Seq[Set[Variable]] = Seq()

    for (v <- neighbours.keys) {
      val component = crawlVariables(v, neighb, visited)
      if (component.nonEmpty) {
        components +:= component
        visited ++= component
      }
    }

    for (vars <- components) yield {
      val c = clauses.filter(c => c.vars.exists(vars))
      val p = pb.filter(c => c.vars.exists(vars))

      (c, p) match {
        case (Seq(singleClause), Seq()) => new ClauseConstraint(singleClause)
        case (clauses, pb)              => new SAT(vars.toArray, clauses, pb)
      }
    }
  }

  def crawlVariables(root: Variable, neighbours: Map[Variable, Set[Variable]], visited: Set[Variable]): Set[Variable] = {
    if (visited(root)) {
      Set()
    } else {
      neighbours(root).foldLeft(visited + root) {
        (visit, n) => visit ++ crawlVariables(n, neighbours, visit)
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
            case SumMode.SumLE =>
              solver.addPseudoBoolean(
                new VecInt(p.vars.map(v => position(v).head + 1).toArray),
                new Vec(p.coefs.map(BigInteger.valueOf(_)).toArray),
                false,
                BigInteger.valueOf(p.constant))
            case SumMode.SumEQ =>
              solver.addPseudoBoolean(
                new VecInt(p.vars.map(v => position(v).head + 1).toArray),
                new Vec(p.coefs.map(BigInteger.valueOf(_)).toArray),
                false,
                BigInteger.valueOf(p.constant))
              solver.addPseudoBoolean(
                new VecInt(p.vars.map(v => position(v).head + 1).toArray),
                new Vec(p.coefs.map(BigInteger.valueOf(_)).toArray),
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