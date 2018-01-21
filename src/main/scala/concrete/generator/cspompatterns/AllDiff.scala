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

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
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
    val expressions = new mutable.HashMap[CSPOMExpression[_], Int]
    val expr = new ArrayBuffer[CSPOMExpression[_]]
    val neighbors = new ArrayBuffer[Set[Int]]
    for (c <- cspom.constraints; args <- DIFF_CONSTRAINT(c); Seq(a1, a2) <- args.combinations(2)) {
      val i1 = expressions.getOrElseUpdate(a1, {
        val i = neighbors.length
        expr += a1
        neighbors += Set()
        i
      })
      val i2 = expressions.getOrElseUpdate(a2, {
        val i = neighbors.length
        expr += a2
        neighbors += Set()
        i
      })
      neighbors(i1) += i2
      neighbors(i2) += i1
    }

    val ranges = expr.zipWithIndex.collect {
      case (IntExpression(e), i) => i -> IntExpression.implicits.ranges(e)
    }

    for (Seq((i1, r1), (i2, r2)) <- ranges.combinations(2) if (r1 & r2).isEmpty) {
      logger.info(s"Additional neighbors ${expr(i1)} != ${expr(i2)}")
      neighbors(i1) += i2
      neighbors(i2) += i1
    }

    val cliques = BronKerbosch2(neighbors).sortBy(-_.size)

    cliques.foldLeft(Delta.empty) { (d, c) =>
      d ++ newAllDiff(c.map(expr), cspom)
    }
  }

  def BronKerbosch2(neighbors: IndexedSeq[Set[Int]]): Seq[Set[Int]] = {

    var C: Seq[Set[Int]] = Seq()

    def recurse(r: Set[Int], p: Set[Int], x: Set[Int]): Unit = {

      if (p.isEmpty && x.isEmpty) {
        // report R as a max clique
        C :+= r
      } else {
        //choose a pivot vertex u in P ⋃ X
        val u: Int = p.headOption.getOrElse(x.head)

        var pr = p
        var xr = x
        for (v <- pr if !neighbors(u).contains(v)) {
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


}
