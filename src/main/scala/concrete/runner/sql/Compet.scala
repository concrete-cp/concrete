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

  val problemQuery = {
    sql"""
        SELECT "problemId", display, "nbVars", "nbCons", nature, coalesce(string_agg("problemTag", ','), '') as tags
        FROM "Problem" LEFT JOIN "ProblemTag" USING ("problemId")
        -- WHERE name ~ '^instances/mznc2017'
        GROUP BY "problemId"
        """.as[Problem]
  }

  val executionQuery = {
    sql"""
    SELECT "problemId", "configId", iteration, status, solution, totalTime('{solver.searchCpu, solver.preproCpu, runner.loadTime}', "executionId")/1e3
    FROM "Execution" where iteration=0 -- and "configId" >= 0
    """.as[Execution]
  }

  val configQuery = sql"""SELECT "configId", config, description FROM "Config" -- WHERE "configId" IN (#${nature.mkString(", ")})"""
    .as[Config]


  val groupedExecutions = DB.run(problemQuery).flatMap { problems =>
    val p = problems.map(q => q.problemId -> q).toMap

    var executions: Seq[ExecutionResult] = Nil

    DB.stream(executionQuery).mapResult(e => ExecutionResult(p, e))
      .foreach { e =>
        executions ++:= e
      }
      .map { _ =>
        println(s"${executions.map(_.problem).distinct.size} problems")

        executions
      }


  }
    .map { executions =>
      println(s"${executions.size} executions")
      executions
        .groupBy { e => (e.problem, e.iteration) }
        // .mapValues(_._2))
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
        val c = e1.compareTo(e2)
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
        println(f"${cfgs(e.configId)}\t${e.statistic.getOrElse(Double.NaN)}%.3f\t$e\t${probScores(e.configId)}%.2f")
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
                        statistic: Option[Double])

  class ExecutionResult(val problem: Problem, val configId: Int, val iteration: Int, val result: CompetResult, val statistic: Option[Double]) {

    def compareTo(e: ExecutionResult): Int = {
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


          if (!solved) {
            if (e.solved) -1 else 0
          } else if (!e.solved) {
            1
          } else if (optimal) {
            if (!e.optimal) 1 else 0
          } else if (e.optimal) {
            -1
          } else {
            require(result.result.isDefined, this)
            require(e.result.result.isDefined, this)
            nature match {
              case Minimize(_) => Ordering.Int.compare(e.result.result.get, result.result.get)
              case Maximize(_) => Ordering.Int.compare(result.result.get, e.result.result.get)
            }
          }
      }
    }

    def solved: Boolean = result.solved

    private def optimal = result.optimal

    def nature: Nature = problem.nat

    override def toString = result.toString

  }

  object ExecutionResult {
    def apply(problems: Map[Int, Problem], e: Execution): Option[ExecutionResult] = {
      problems.get(e.problemId).map { problem =>
        val result: CompetResult = {
          problem.nat match {
            case Satisfy if e.status == "SAT1" => SAT
            case _ if e.status == "UNSAT" => UNSAT

            case nature: Optimize =>
              // val variable = nature.variable
              val result =
                e.solution.flatMap(_.split("\n").filter(_.contains("=")).lastOption).map { l =>
                  try {
                    l.split("=|;")(1).trim.toInt
                  } catch {
                    case exc: Exception => throw new IllegalStateException(s"Unable to parse solution from $l", exc)
                  }
                }

              //require(!solved || result.nonEmpty, s"$status but empty result $result")
              if (e.status == "SAT*") {
                Optimal(result.get)
              } else if (result.isDefined) {
                SubOptimal(result.get)
              } else {
                Unknown
              }
            case _ => Unknown
          }

        }
        new ExecutionResult(problem, e.configId, e.iteration, result, e.statistic)

      }

    }
  }


  println()

  for ((c, v) <- scores.toSeq.sortBy(_._2)) {
    println(f"$c. ${cfgs(c)}: $v%.2f")
  }

  println(order.map(c => f"${scores(c)}%.0f").mkString(" & "))
}
