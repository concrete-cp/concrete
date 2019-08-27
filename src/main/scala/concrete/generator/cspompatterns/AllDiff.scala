package concrete.generator.cspompatterns


import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.linear.SumMode
import concrete.generator.SumGenerator
import cspom.compiler.ConstraintCompiler._
import cspom.compiler._
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint}

import scala.util.Random


/**
  * Aggregates cliques of alldifferent constraints. Uses a small tabu search engine
  * to detect max-cliques.
  */
object AllDiff extends ProblemCompiler with LazyLogging {
  type A = Seq[CSPOMVariable[_]]
  val RAND = new Random(0)

  private val neCoefs1 = Seq(-1, 1)

  private val neCoefs2 = Seq(1, -1)

  def apply(cspom: CSPOM): Delta = {
    val edges = ALLDIFF_FUNCTIONS.flatMap(cspom.getConstraints).flatMap(DIFF_CONSTRAINT)

    val expressions = edges.flatten.distinct.toArray
    val neighbors = computeNeighbors(edges, expressions)

    val c = greedy(neighbors) //BronKerbosch2(neighbors)


    val cliques = c.filter(_.size > 2).sortBy(-_.size)


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

  private def greedy(neighbors: Array[Set[Int]]): Seq[Set[Int]] = {
    def expand(currentClique: List[Int], candidates: List[Int]): List[Int] = {
      candidates match {
        case Nil => currentClique
        case head :: tail =>
          expand(head :: currentClique, tail.filter(neighbors(head)))
      }
    }

    var queue = neighbors.indices.toSet
    var cliques: List[Set[Int]] = Nil
    while (queue.nonEmpty) {
      // pick
      val picked = queue.head
      val clique = expand(List(picked), neighbors(picked).toList)
      logger.debug(s"new clique of size ${clique.size}: $clique")
      cliques ::= clique.toSet
      queue --= clique
    }
    cliques
  }

  /**
    * Adds a new all-diff constraint of the specified scope. Any newly subsumed
    * neq/all-diff constraints are removed.
    *
    * @param scope
    */
  private def newAllDiff(scope: Set[CSPOMExpression[_]], problem: CSPOM): Delta = {

    val subsumed = scope.flatMap(problem.deepConstraints).filter(c => isSubsumed(c, scope))
    val allDiff = CSPOMConstraint("alldifferent")(scope.toSeq: _*)
    logger.info("New alldiff: " + allDiff.toString(problem.displayName))

    var delta = addCtr(allDiff, problem)

    if (subsumed.nonEmpty) {
      var removed = 0
      /* Remove newly subsumed neq/alldiff constraints. */

      for (c <- subsumed) {
        removed += 1
        logger.info(s"Removing ${c.toString(problem.displayName)}")
        delta ++= removeCtr(c, problem)
      }
      logger.info(s"removed $removed constraints, ${problem.constraints.size} left")
    } else {
      logger.info(s"alldiff(${scope.map(_.toString(problem.displayName)).mkString(", ")}) does not replace any constraint")
    }
    delta

  }

  private def isSubsumed(c: CSPOMConstraint[_], by: Set[CSPOMExpression[_]]): Boolean = {
    ALLDIFF_CONSTRAINT(c).exists(a =>
      a.forall(by.contains)
    )
  }

  //private val neighborsCache = new WeakHashMap[CSPOMVariable[_], Set[CSPOMVariable[_]]]

  def ALLDIFF_CONSTRAINT(constraint: CSPOMConstraint[_]): Option[Seq[CSPOMExpression[_]]] = {
    if (constraint.function == "alldifferent" && constraint.nonReified && constraint.getSeqParam("except").isEmpty) {
      Some(constraint.arguments)
    } else if (constraint.function == "eq" && constraint.result.isFalse) {
      Some(constraint.arguments)
    } else if (constraint.function == "ne" && constraint.nonReified) {
      Some(constraint.arguments)
    } else if (constraint.function == "sum" && constraint.nonReified) {
      val (vars, coefs, constant, mode) = SumGenerator.readCSPOM(constraint)

      if (mode == SumMode.NE && constant == 0 && (coefs == neCoefs1 || coefs == neCoefs2)) {
        Some(vars)
      } else {
        None
      }
    } else None
  }

  def ALLDIFF_FUNCTIONS: Seq[String] = Seq("alldifferent", "eq", "ne", "sum")

  def DIFF_CONSTRAINT(constraint: CSPOMConstraint[_]): Option[Seq[CSPOMExpression[_]]] =
    ALLDIFF_CONSTRAINT(constraint).orElse {
      if (constraint.function == "sum" && constraint.nonReified) {
        val (vars, coefs, constant, mode) = SumGenerator.readCSPOM(constraint)

        PartialFunction.condOpt(mode) {
          case SumMode.LT | SumMode.GT
            if constant == 0 && (coefs == neCoefs1 || coefs == neCoefs2) =>
            vars
        }
      } else None
    }

  private def BronKerbosch2(neighbors: IndexedSeq[Set[Int]]): Seq[Set[Int]] = {

    var C: Seq[Set[Int]] = Seq()

    def recurse(r: Set[Int], p: Set[Int], x: Set[Int]): Unit = {

      if (p.isEmpty && x.isEmpty) {
        // report R as a max clique
        logger.warn(s"new clique of size ${r.size}: $r")
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
}
