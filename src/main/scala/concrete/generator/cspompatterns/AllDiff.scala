package concrete.generator.cspompatterns

import com.typesafe.scalalogging.LazyLogging
import concrete.CSPOMDriver
import concrete.constraint.linear.{SumLT, SumNE}
import concrete.generator.SumGenerator
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.{ConstraintCompiler, Delta}
import cspom.util.{IntInterval, RangeSet}
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint}

import scala.util.Random

/**
  * Removes constants from alldifferent constraints
  */
object AllDiffConstant extends ConstraintCompiler {
  type A = (Seq[Int])

  def selfPropagation = true

  override def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[A] = {

    if (c.function == 'alldifferent) {
      require(c.result == CSPOMConstant(true))
      val constants = c.arguments.collect {
        case c: CSPOMConstant[_] => c.intValue
      }
      if (constants.isEmpty) {
        None
      } else {
        Some(constants)
      }
    } else {
      None
    }

  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, constants: A): Delta = {
    val variables = constraint.arguments.collect {
      case v: IntVariable => v
    }
    require(constants.distinct.size + variables.size == constraint.arguments.size)

    val constantSet = RangeSet(constants.map(k => IntInterval.singleton(k)))

    val filt = variables.map { v: IntVariable =>
      applyDomain(v, v.domain -- constantSet)
      //if (r == v.domain) v else IntVariable(r)
    }

    var delta = removeCtr(constraint, problem)

    if (filt.length > 1) {
      delta ++= addCtr(CSPOMDriver.allDifferent(filt: _*), problem)
    }

    for ((v, f) <- (variables, filt).zipped) {
      delta ++= replace(v, f, problem)
    }

    delta
  }
}

/**
  * Aggregates cliques of alldifferent constraints. Uses a small tabu search engine
  * to detect max-cliques.
  */
object AllDiff extends ConstraintCompiler with LazyLogging {
  type A = Seq[CSPOMVariable[_]]
  val RAND = new Random(0)


  private val neCoefs1 = Seq(-1, 1)

  private val neCoefs2 = Seq(1, -1)

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM): Option[List[CSPOMVariable[_]]] = {
    DIFF_CONSTRAINT(constraint).flatMap { args =>
      // println(constraint)
      // println(constraint.toString(problem.displayName))
      val clique = expand(args.collect { case v: CSPOMVariable[_] => v }.toSet, problem)
      if (clique.size > args.size) {
        Some(clique)
      } else {
        None
      }
    }
  }

  /**
    * If constraint is part of a larger clique of inequalities, replace it by a
    * larger all-diff constraint.
    *
    * @param constraint
    */
  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, clique: Seq[CSPOMVariable[_]]): Delta = {
    newAllDiff(clique, problem)
  }

  /**
    * Adds a new all-diff constraint of the specified scope. Any newly subsumed
    * neq/all-diff constraints are removed.
    *
    * @param scope
    */
  private def newAllDiff(scope: Seq[SimpleExpression[_]], problem: CSPOM): Delta = {

    val allDiff = CSPOMConstraint('alldifferent)(scope: _*)

    var delta = Delta()

    if (!scope.flatMap(problem.constraints).exists(c => isSubsumed(allDiff, c))) {
      logger.debug("New alldiff: " + allDiff.toString(problem.displayName))
      delta ++= addCtr(allDiff, problem)

      var removed = 0
      /* Remove newly subsumed neq/alldiff constraints. */

      for (
        v <- scope;
        c <- problem.deepConstraints(v) if isSubsumed(c, allDiff)
      ) {
        removed += 1
        delta ++= removeCtr(c, problem)
      }
      logger.debug("removed " + removed + " constraints, " + problem.constraints.size + " left")
    }
    delta
  }

  //private val neighborsCache = new WeakHashMap[CSPOMVariable[_], Set[CSPOMVariable[_]]]

  private def isSubsumed(c: CSPOMConstraint[_], by: CSPOMConstraint[_]): Boolean = {
    (by ne c) && DIFF_CONSTRAINT(by).isDefined && {

      ALLDIFF_CONSTRAINT(c).exists { a =>
        a.forall(by.arguments.contains)
      }
    }
  }

  def DIFF_CONSTRAINT(constraint: CSPOMConstraint[_]): Option[Seq[CSPOMExpression[_]]] =
    ALLDIFF_CONSTRAINT(constraint).orElse {
      if (constraint.function == 'sum && constraint.result.isTrue) {
        val (vars, coefs, constant, mode) = SumGenerator.readCSPOM(constraint)

        if (mode == SumLT && constant == 0 && (coefs == neCoefs1 || coefs == neCoefs2)) {
          Some(vars)
        } else {
          None
        }
      } else None
    }

  def ALLDIFF_CONSTRAINT(constraint: CSPOMConstraint[_]): Option[Seq[CSPOMExpression[_]]] = {
    if (constraint.function == 'alldifferent && constraint.result.isTrue) {
      Some(constraint.arguments)
    } else if (constraint.function == 'eq && constraint.result.isFalse) {
      Some(constraint.arguments)
    } else if (constraint.function == 'ne && constraint.result.isTrue) {
      Some(constraint.arguments)
    } else if (constraint.function == 'sum && constraint.result.isTrue) {
      val (vars, coefs, constant, mode) = SumGenerator.readCSPOM(constraint)

      if (mode == SumNE && constant == 0 && (coefs == neCoefs1 || coefs == neCoefs2)) {
        Some(vars)
      } else {
        None
      }
    } else None
  }

  def selfPropagation = false

  def neighbors(v: CSPOMVariable[_], problem: CSPOM): Set[CSPOMVariable[_]] = {
    //    println("****** constraints with " + v)
    //    problem.deepConstraints(v).foreach(c=> println(c.toString(problem.displayName)))
    problem.deepConstraints(v)
      .iterator
      .flatMap(DIFF_CONSTRAINT)
      .flatten
      .collect {
        case v: CSPOMVariable[_] => v
      }
      .toSet
    //      problem.deepConstraints(v).flatMap(DIFF_CONSTRAINT).flatten.collect {
    //        case v: IntVariable => v
    //      }.toSet - v)

  }

  private def expand(base: Set[CSPOMVariable[_]], problem: CSPOM) = {

    val expanded = base.toList

    val candidates = neighbors(expanded.head, problem) -- base

    var allNeighbors = expanded.tail.map(v => neighbors(v, problem))

    expanded ++ (for (c <- candidates if allNeighbors.forall(n => n.contains(c))) yield {
      allNeighbors ::= neighbors(c, problem)
      c
    })

  }

}
