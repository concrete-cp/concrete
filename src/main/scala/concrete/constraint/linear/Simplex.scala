package concrete.constraint.linear

import org.apache.commons.math3.optim.linear.{ LinearConstraint => ApacheConstraint }
import org.apache.commons.math3.optim.linear.LinearObjectiveFunction
import org.apache.commons.math3.optim.linear.Relationship
import org.apache.commons.math3.optim.linear.SimplexSolver
import concrete.Variable
import concrete.constraint.Constraint
import concrete.generator.LinearConstraint
import org.apache.commons.math3.optim.linear.LinearConstraintSet
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType
import concrete.Contradiction
import org.apache.commons.math3.optim.linear.NoFeasibleSolutionException
import cspom.UNSATException
import concrete.ProblemState
import concrete.Outcome
import org.apache.commons.math3.util.FastMath

import concrete.ParameterManager
import cspom.Statistic
import cspom.util.BitVector
import concrete.util.Interval
import scala.collection.JavaConverters._
import concrete.constraint.BoundRemovals
import concrete.Domain
import concrete.cluster.ConnectedComponents

object Simplex {
  def apply(lin: Seq[LinearConstraint], pm: ParameterManager): Seq[Constraint] = {
    val components = ConnectedComponents(lin)
    for (lin <- components) yield {
      println(lin.size)
      if (lin.size == 1) {
        val l = lin.head
        Linear(l.constant, l.factors.toArray, l.vars.toArray, l.mode, pm)
      } else {
        val varArray = lin.flatMap { _.vars }.distinct.toArray
        val varMap = varArray.zipWithIndex.toMap

        val constraints = for (l <- lin) yield {
          val rel = l.mode match {
            case SumLE => Relationship.LEQ
            case SumEQ => Relationship.EQ
            case _     => throw new UnsupportedOperationException
          }
          new ApacheConstraint(factRow(varMap, l).map(_.toDouble), rel, l.constant.toDouble)
        }
        // println(constraints.map(c => s"${c.getCoefficients} ${c.getRelationship} ${c.getValue}").mkString("\n"))
        new Simplex(constraints, varArray)
      }
    }

  }

  private def factRow(map: Map[Variable, Int], l: LinearConstraint): Array[Int] = {
    val fact = Array.fill(map.size)(0)
    for ((f, v) <- (l.factors, l.vars).zipped) {
      fact(map(v)) = f
    }
    fact
  }

  val DEFAULT_EPSILON = 1.0e-6

  @Statistic
  var solves = 0
}

/**
 * @author vion
 */
class Simplex(
  constraints: Seq[ApacheConstraint],
  variables: Array[Variable]) extends Constraint(variables)
    with BoundRemovals[Unit] {

  // Each constraint covers all variables
  require(constraints.forall(_.getCoefficients.getDimension == arity))

  def getEvaluation(ps: ProblemState) =
    variables.size * constraints.size

  def check(tuple: Array[Int]): Boolean = {
    constraints.forall { c =>
      new LinearLe(c.getValue.toInt, c.getCoefficients.toArray.map(_.toInt), null, null).check(tuple)
    }
  }

  val diagonal = Array.tabulate(arity) { p =>
    val v = Array.fill(arity)(0.0)
    v(p) = 1.0
    v
  }

  val objVec = Array.fill(arity)(0.0)

  def init(ps: concrete.ProblemState): Outcome =
    ps.updateState(this, (null, ()))

  val solver = new SimplexSolver(Simplex.DEFAULT_EPSILON)

  val graph: Array[BitVector] = {
    val g = Array.fill(arity)(BitVector.empty)
    for (c <- constraints) {
      val scope = c.getCoefficients.sparseIterator.asScala.map(_.getIndex).toSeq
      for (Seq(p1, p2) <- scope.combinations(2)) {
        g(p1) += p2
        g(p2) += p1
      }
    }
    g
  }

  //println(graph.mkString("\n"))

  //  val graph = Array.tabulate(arity)(p => BitVector.filled(arity) - p)

  val allConstraints =
    Array.ofDim[ApacheConstraint](2 * arity) ++ this.constraints

  def reviseBounds(
    ps: concrete.ProblemState,
    mod: BitVector,
    doms: Array[Domain],
    data: Unit): Outcome = {
    //println(s"% Revising $id due to $mod")

    val queue = mod.traversable.view.map(graph).reduce(_ | _).filter(p => !doms(p).isAssigned)

    for (p <- 0 until arity) {
      val dom = doms(p)
      val diag = diagonal(p)
      allConstraints(2 * p) = new ApacheConstraint(diag, Relationship.GEQ, dom.head)
      allConstraints(2 * p + 1) = new ApacheConstraint(diag, Relationship.LEQ, dom.last)
    }

    filter(ps, doms, queue, queue.nextSetBit(0))
  }

  @annotation.tailrec
  private def filter(ps: ProblemState, doms: Array[Domain], queue: BitVector, p: Int): Outcome =
    if (p < 0) ps
    else {
      objVec(p) = 1
      val objective = new LinearObjectiveFunction(objVec, 0)

      val cs = new LinearConstraintSet(allConstraints: _*)

      val filt = try {
        Simplex.solves += 1
        val h = solver.optimize(objective, cs, GoalType.MINIMIZE).getValue
        Simplex.solves += 1
        val l = solver.optimize(objective, cs, GoalType.MAXIMIZE).getValue

        val nh = h - Simplex.DEFAULT_EPSILON
        val nl = l + Simplex.DEFAULT_EPSILON

        Interval(math.ceil(nh).toInt, math.floor(nl).toInt)
      } catch {
        case e: NoFeasibleSolutionException => return Contradiction
      } finally {
        objVec(p) = 0
      }

      val dom = doms(p)
      val newDom = dom & filt

      if (newDom eq dom) {
        //println(s"% $p : $dom -> NOP")
        val newQueue = queue - p
        filter(ps, doms, newQueue, newQueue.nextOrLoop(p))
      } else if (newDom.isEmpty) {
        //println(s"% $p : Contradiction")
        Contradiction
      } else {
        //println(s"% $p : $dom -> $newDom")
        val diag = diagonal(p)
        allConstraints(2 * p) = new ApacheConstraint(diag, Relationship.GEQ, newDom.head)
        allConstraints(2 * p + 1) = new ApacheConstraint(diag, Relationship.LEQ, newDom.last)

        //val newQueue = (BitVector.filled(arity) - p).filter(p => !ps.assigned(scope(p))) 
        val newQueue = (queue - p) | graph(p).filter(p => !ps.assigned(scope(p)))

        doms(p) = newDom

        filter(
          ps.updateDomNonEmpty(scope(p), newDom),
          doms,
          newQueue,
          newQueue.nextOrLoop(p))
      }

    }

  def simpleEvaluation: Int = 3
}