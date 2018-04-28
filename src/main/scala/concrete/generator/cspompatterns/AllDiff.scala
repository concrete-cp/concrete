package concrete.generator.cspompatterns


import com.typesafe.scalalogging.LazyLogging
import concrete.CSPOMDriver
import concrete.constraint.linear.{SumLT, SumNE}
import concrete.generator.SumGenerator
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.{ConstraintCompiler, Delta, ProblemCompiler}
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

    if (filt.lengthCompare(1) > 0) {
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
object AllDiff extends ProblemCompiler with LazyLogging {
  type A = Seq[CSPOMVariable[_]]
  val RAND = new Random(0)


  private val neCoefs1 = Seq(-1, 1)

  private val neCoefs2 = Seq(1, -1)

  //  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM): Option[List[CSPOMVariable[_]]] = {
  //    DIFF_CONSTRAINT(constraint).flatMap { args =>
  //      // println(constraint)
  //      // println(constraint.toString(problem.displayName))
  //      val clique = expand(args.collect { case v: CSPOMVariable[_] => v }.toSet, problem)
  //      if (clique.size > args.size) {
  //        Some(clique)
  //      } else {
  //        None
  //      }
  //    }
  //  }

  //  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, clique: Seq[CSPOMVariable[_]]): Delta = {
  //    newAllDiff(clique, problem)
  //  }

  def apply(cspom: CSPOM): Delta = {
    val edges = cspom.constraints.flatMap(DIFF_CONSTRAINT).toSeq
    val expressions = edges.flatten.distinct.toArray
    val neighbors = computeNeighbors(edges, expressions)

    // addNeighbors(neighbors, expressions)

    val cliques = BronKerbosch2(neighbors).filter(_.size > 2).sortBy(-_.size)

    cliques.foldLeft(Delta.empty) { (d, c) =>
      d ++ newAllDiff(c.map(expressions), cspom)
    }
  }

  private def computeNeighbors(edges: Seq[Seq[CSPOMExpression[_]]], expressions: Array[CSPOMExpression[_]]): Array[Set[Int]] = {
    val indices = expressions.zipWithIndex.toMap
    val neighbors = Array.fill(expressions.length)(Set.newBuilder[Int])
    for (e <- edges; Seq(a1, a2) <- e.combinations(2)) {
      val i1 = indices(a1)
      val i2 = indices(a2)
      neighbors(i1) += i2
      neighbors(i2) += i1
    }
    neighbors.map(_.result())
  }

  private def BronKerbosch2(neighbors: IndexedSeq[Set[Int]]): Seq[Set[Int]] = {

    var C: Seq[Set[Int]] = Seq()

    def recurse(r: Set[Int], p: Set[Int], x: Set[Int]): Unit = {

      if (p.isEmpty && x.isEmpty) {
        // report R as a max clique
        logger.debug(s"new clique of size ${r.size}: $r")
        C :+= r
      } else {
        //choose a pivot vertex u in P ⋃ X
        val u: Int = p.headOption.getOrElse(x.head)

        var pr = p
        var xr = x
        for (v <- p if !neighbors(u).contains(v)) {
          // BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
          recurse(r + v, pr.intersect(neighbors(v)), xr.intersect(neighbors(v)))
          pr -= v
          xr += v
        }
      }
    }

    recurse(Set(), neighbors.indices.toSet, Set())
    C
  }

  /**
    * Adds a new all-diff constraint of the specified scope. Any newly subsumed
    * neq/all-diff constraints are removed.
    *
    * @param scope
    */
  private def newAllDiff(scope: Set[CSPOMExpression[_]], problem: CSPOM): Delta = {

    val subsumed = scope.flatMap(problem.deepConstraints).filter(c => isSubsumed(c, scope))

    if (subsumed.nonEmpty) {
      val allDiff = CSPOMConstraint('alldifferent)(scope.toSeq: _*)
      logger.info("New alldiff: " + allDiff.toString(problem.displayName))
      var delta = addCtr(allDiff, problem)

      var removed = 0
      /* Remove newly subsumed neq/alldiff constraints. */

      for (c <- subsumed) {
        removed += 1
        logger.info(s"Removing ${c.toString(problem.displayName)}")
        delta ++= removeCtr(c, problem)
      }
      logger.info(s"removed $removed constraints, ${problem.constraints.size} left")
      delta
    } else {
      logger.info(s"alldiff(${scope.map(_.toString(problem.displayName)).mkString(", ")}) does not replace any constraint")
      Delta()
    }

  }

  //private val neighborsCache = new WeakHashMap[CSPOMVariable[_], Set[CSPOMVariable[_]]]

  private def isSubsumed(c: CSPOMConstraint[_], by: Set[CSPOMExpression[_]]): Boolean = {
    (c.flattenedScope.size < by.size) && ALLDIFF_CONSTRAINT(c).exists { a =>
      a.forall(by.contains)
    }
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

  def selfPropagation = false


}
