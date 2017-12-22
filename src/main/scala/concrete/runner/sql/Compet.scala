package concrete.runner.sql

import com.typesafe.config.ConfigFactory
import slick.jdbc.GetResult
import slick.jdbc.PostgresProfile.api._

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.xml.{Node, Text}

object Compet extends App {
  lazy val systemConfig = ConfigFactory.load //defaults from src/resources
  lazy val DB =
    Database.forConfig("database", systemConfig)
  val nature = args.toList
  val configs = nature.map(_.toInt)
  val configsMap = configs.zipWithIndex.toMap


  implicit val getProblemResult: GetResult[Problem] = GetResult(r => Problem(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))

  //var d = Array.ofDim[Int](configs.size, configs.size)

  // val ignoreNaN = true

  implicit val getExecutionResult: GetResult[Execution] = GetResult(r => Execution(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))

  implicit val getConfigResult: GetResult[Config] = GetResult(r => Config(r.<<, r.<<, r.<<))

  val problemQuery = sql"""
        SELECT "problemId", display, "nbVars", "nbCons", nature, coalesce(string_agg("problemTag", ','), '') as tags
        FROM "Problem" LEFT JOIN "ProblemTag" USING ("problemId")
        -- WHERE name ~ '^instances/mznc2017'
        GROUP BY "problemId"
        """.as[Problem]
  val executionQuery = sql"""
    SELECT "problemId", "configId", iteration, status, solution, totalTime('{solver.searchCpu, solver.preproCpu, runner.loadTime}', "executionId")/1e3
    FROM "Execution" where iteration=0 -- and "configId" not in(0)
    """.as[Execution]
  val configQuery = sql"""SELECT "configId", config, description FROM "Config" -- WHERE "configId" IN (#${nature.mkString(", ")})"""
    .as[Config]
  val groupedExecutions = for {
    problems <- DB.run(problemQuery)
    p = problems.map(q => q.problemId -> q).toMap
    executions <- DB.run(executionQuery)
  } yield {
    executions.flatMap { e =>
      p.get(e.problemId).map { problem => (problem, e) }
    }
      .groupBy { case (prob, exec) => (prob, exec.iteration) }
      .mapValues(_.map(_._2))
      .toSeq
      .sortBy { case ((prob, iter), _) => (prob.problem, iter) }
  }
  val order = // (-1 to -21 by -1) ++
    Array(66, 70, 71)
  val fut = for (
    pe <- groupedExecutions; cfgsSeq <- DB.run(configQuery);
    cfgs = cfgsSeq.map(c => c.configId -> c.desc).toMap
  ) yield {

    val scores = new mutable.HashMap[Int, Double].withDefaultValue(0)

    val catScores = new mutable.HashMap[String, collection.mutable.Map[Int, Double]]

    for (((problem, iteration), executions) <- pe) {
      println()
      println(s"${problem.problem}-$iteration ${problem.nat}")

      val probScores = new mutable.HashMap[Int, Double].withDefaultValue(0)

      for (Seq(e1, e2) <- executions.combinations(2)) {
        val c = e1.compareTo(e2, problem.nat)
        if (c > 0) {
          probScores(e1.configId) += 1
        }
        else if (c < 0) {
          probScores(e2.configId) += 1
        }
        else {
          val s1 = e1.statistic.getOrElse(1000.0)
          val s2 = e2.statistic.getOrElse(1000.0)
          val score = s2 / (s1 + s2)

          if (e1.solved) {
            probScores(e1.configId) += score
          }
          if (e2.solved) {
            probScores(e2.configId) += 1 - score
          }
        }

      }

      for (e <- executions.sortBy(e => probScores(e.configId))) {
        println(f"${cfgs(e.configId)}\t${e.statistic.getOrElse(Double.NaN)}%.3f\t${e.toString(problem.nat)}\t${probScores(e.configId)}%.2f")
      }

      for ((c, s) <- probScores) {
        scores(c) += s
      }

      for ((c, s) <- probScores; tag <- problem.tags) {
        val scores = catScores.getOrElseUpdate(tag, new mutable.HashMap[Int, Double].withDefaultValue(0))
        scores(c) += s
      }

    }

    println()
    for ((cat, scores) <- catScores.toSeq.sortBy(_._1)) {
      // println()
      print(cat + " & ")
      //      for ((conf, score) <- scores.toSeq.sortBy(_._2)) {
      //        println(f"${cfgs(conf)}: $score%.2f")
      //      }

      val best = scores.toSeq.map(_._2).sorted.reverse.drop(2).headOption.getOrElse(0.0) * .95
      //println(scores)
      //println(s"best = ${scores.toSeq.map(_._2).sorted.reverse}")

      println(order.map(c => f"${if (scores(c) > best) "\\bf " else ""} ${scores(c)}%.0f").mkString(" & ") + " \\\\")
    }

    (cfgs, scores) //.map { case (k, v) => s"$k. ${cfgs(k)}" -> v }

  }
  val (cfgs, scores) = Await.result(fut, Duration.Inf)

  def attributeEquals(name: String, value: String)(node: Node): Boolean = {
    node.attribute(name).get.contains(Text(value))
  }

  sealed trait Failure

  sealed trait Nature

  sealed trait Result {
    def solved: Boolean

    def optimal: Boolean

    def result: Option[Int]
  }

  sealed abstract class Optimize(_variable: String) extends Nature {
    def variable: Seq[String] = _variable.split("\\|\\|")
  }

  case class Problem(
                      problemId: Int,
                      problem: String,
                      nbVars: Int,
                      nbCons: Int,
                      nature: String,
                      _tags: String) {
    lazy val tags: Seq[String] = _tags.split(",")
    lazy val nat = Nature(nature)
  }

  case class Config(
                     configId: Int,
                     config: String,
                     desc: String)

  case class Execution(
                        problemId: Int,
                        configId: Int,
                        iteration: Int,
                        status: String,
                        solution: Option[String],
                        statistic: Option[Double]) {

    def compareTo(e: Execution, nature: Nature): Int = {
      nature match {
        case Satisfy =>
          if (solved) {
            if (e.solved) {
              0
            } else {
              1
            }
          } else if (e.solved) {
            -1
          } else {
            0
          }
        case nature: Optimize =>

          val r1 = latestResult(nature.variable)
          val r2 = e.latestResult(nature.variable)

          if (!r1.solved) {
            if (r2.solved) -1 else 0
          } else if (!r2.solved) {
            1
          } else if (r1.optimal) {
            if (!r2.optimal) 1 else 0
          } else if (r2.optimal) {
            -1
          } else {
            require(r1.result.isDefined, r1)
            require(r2.result.isDefined, r2)
            nature match {
              case Minimize(_) => Ordering.Int.compare(r2.result.get, r1.result.get)
              case Maximize(_) => Ordering.Int.compare(r1.result.get, r2.result.get)
            }
          }
      }
    }

    def latestResult(variable: Seq[String]): Result = {
      val result = solution.flatMap(_.split("\n").filter(s => variable.exists(s.startsWith)).lastOption).map(_.split("=|;")(1).trim.toInt)
      //require(!solved || result.nonEmpty, s"$status but empty result $result")
      if (complete) {
        Complete(result)
      } else {
        Incomplete(result)
      }
    }

    def solved: Boolean = {
      complete || status == "SAT1" || solution.exists(_.contains("----------"))
    }

    def complete: Boolean = {
      status == "SAT*" || status == "UNSAT"
    }

    def toString(nature: Nature): String = {
      nature match {
        case Satisfy =>
          if (status == "SAT1")
            "SAT"
          else if (status == "UNSAT")
            "UNSAT"
          else
            "UNK"
        case o: Optimize =>
          latestResult(o.variable).toString

      }
    }

  }

  case class Minimize(v: String) extends Optimize(v)

  case class Maximize(v: String) extends Optimize(v)

  case class Incomplete(result: Option[Int]) extends Result {
    override def toString: String = result.map(_.toString).getOrElse("UNK")

    def solved: Boolean = result.isDefined

    def optimal = false

  }

  case class Complete(result: Option[Int]) extends Result {
    override def toString: String = result.map(_ + "*").getOrElse("UNSAT")

    def solved = true

    def optimal = true
  }

  //  {
  //    val configsQ = sql""" 
  //        SELECT "configId", config
  //        FROM "Config" 
  //        WHERE "configId" IN (#${nature.mkString(", ")}) 
  //        """.as[(Int, String)]
  //
  //    val configDisplay = new Array[String](nature.size)
  //
  //    val configs = Await.result(DB.run(configsQ), Duration.Inf)
  //
  //    for ((cId, display) <- configs) configDisplay(configsMap(cId)) = display
  //    println("\t" + configDisplay.mkString("\t"))
  //  }



  case object Success extends Failure

  case object Timeout extends Failure

  case object OOM extends Failure

  case object Stalled extends Failure

  case object UnknownError extends Failure

  object Nature {
    def apply(nature: String): Nature = {
      nature.split(" ") match {
        case Array("satisfy") => Satisfy
        case Array("minimize", obj) => Minimize(obj)
        case Array("maximize", obj) => Maximize(obj)
      }
    }
  }

  case object Satisfy extends Nature

  println()

  for ((c, v) <- scores.toSeq.sortBy(_._2)) {
    println(f"$c. ${cfgs(c)}: $v%.2f")
  }

  println(order.map(c => f"${scores(c)}%.0f").mkString(" & "))
}
