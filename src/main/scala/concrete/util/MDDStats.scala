/**
 * CSPFJ - CSP solving API for Java)
 * Copyright (C) 2006 Julien VION
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package concrete.util;

import com.typesafe.scalalogging.LazyLogging
import concrete.runner.sql.MyPGDriver.api._
import concrete.runner.sql.SQLWriter
import concrete.generator.ExtensionGenerator
import concrete.runner.sql.Table2
import cspom.StatisticsManager
import concrete.generator.cspompatterns.ConcretePatterns
import concrete.generator.cspompatterns.XCSPPatterns
import cspom.CSPOM
import cspom.compiler.CSPOMCompiler
import concrete.generator.cspompatterns.FZPatterns
import cspom.extension.MDD
import cspom.extension.Relation
import cspom.StatisticsManager._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import concrete.ParameterManager
import concrete.Problem
import concrete.Solver
import concrete.UNSAT
import scala.math.BigInt.int2bigInt
import scala.reflect.runtime.universe
import concrete.constraint.extension.BDDC
import concrete.constraint.extension.MDDC
import concrete.constraint.extension.ReduceableExt

object MDDStats extends App with LazyLogging {
  def apply(prob: Problem, params: ParameterManager): MDDStats =
    new MDDStats(prob, params)

  val pm = new ParameterManager
  pm("solver") = classOf[MDDStats]

  val eg = new ExtensionGenerator(pm)

  var ea = List[Double]()
  var la = List[BigInt]()
  var ka = List[Double]()
  var da = List[Double]()
  var ba = List[Double]()
  var nba = List[Double]()

  val query = Table2.DB
    .run {
      SQLWriter.problemTag
        .filter(_.problemTag === "xp-table-bdd")
        .join(SQLWriter.problems)
        .on { (tag, problem) => tag.problemId === problem.problemId }
        .map { case (_, problem) => problem.name }
        .result
    }

  val problems = Await.result(query, Duration.Inf)

  println(s"${problems.size} instances")

  for (a <- problems) {
    logger.warn(s"Loading $a")
    //print(s"$a\t")
    val cspom = CSPOM.load(s"/home/vion/xp-table/$a").get
    if (a.contains("fzn")) {
      CSPOMCompiler.compile(cspom, FZPatterns())
    } else if (a.contains("xml")) {
      logger.warn("Compiling XCSP")
      CSPOMCompiler.compile(cspom, XCSPPatterns())
    }
    logger.warn("Compiling for Concrete")
    CSPOMCompiler.compile(cspom, ConcretePatterns(pm))
    logger.warn("Computing stats")
    var edges = List[Int]()
    var lambdas = List[BigInt]()
    var ks = List[Int]()
    var ds = List[Double]()
    var bdds = List[Int]()


    for {
      c <- cspom.constraints
      if (c.function == 'extension && c.arguments.size > 2)
      g <- c.getParam[Relation[Int]]("relation")

    } {
      val r = g match {
        case m: MDD[Int] => m.reduce()
      }
      edges ::= r.edges
      lambdas ::= r.lambda
      ks ::= c.arguments.size
      ds :::= c.arguments.map(_.searchSpace).toList

      val bdd = eg.cspomMDDtoBDD(r)
      bdds ::= bdd.reduce().edges(10)
    }

    if (ds.nonEmpty) {

      da ::= average(ds)
      print(s"${da.head}\t")

      ka ::= average(ks)
      print(s"${ka.head}\t")

      nba ::= ds.size
      print(s"${nba.head}\t")

      la ::= lambdas.sum / lambdas.size
      print(s"${la.head}\t")

      ea ::= average(edges)
      print(s"${ea.head}\t")

      ba ::= average(bdds)
      print(s"${ba.head}")
      //    val solver = Solver(cspom, pm).get
      //    solver.hasNext
    }
    println()
  }

  println()
  println(s"${average(da)}\t${average(ka)}\t${averageBigInt(la)}\t${average(ea)}\t${average(ba)}")
  println(s"${stDev(da)}\t${stDev(ka)}\t${stDevBigInt(la)}\t${stDev(ea)}\t${stDev(ba)}")

}

final class MDDStats(prob: Problem, params: ParameterManager) extends Solver(prob, params) {

  def nextSolution() = {
    val state = problem.initState.toState

    val edges =
      prob.constraints.collect {
        case c: ReduceableExt => state(c).edges
        case c: MDDC          => c.mdd.edges
        case c: BDDC          => c.bdd.edges
      }

    val lambdas =
      prob.constraints.collect {
        case c: ReduceableExt => state(c).lambda
        case c: MDDC          => c.mdd.lambda
        case c: BDDC          => c.bdd.lambda
      }

    val ks =
      prob.constraints.collect {
        case c: ReduceableExt => c.arity
        case c: MDDC          => c.arity
        case c: BDDC          => c.arity
      }

    val ds =
      prob.constraints.collect {
        case c: ReduceableExt => state.doms(c.scope)
        case c: MDDC          => state.doms(c.scope)
        case c: BDDC          => state.doms(c.scope)
      }
        .flatten
        .map(_.size)

    print(s"${StatisticsManager.average(ds)}\t")
    print(s"${StatisticsManager.average(ks)}\t")
    print(s"${StatisticsManager.average(edges)}\t")
    println(s"${StatisticsManager.average(lambdas)}")

    UNSAT
  }
  override def toString = "stats"

  def reset(): Unit = {}

}
