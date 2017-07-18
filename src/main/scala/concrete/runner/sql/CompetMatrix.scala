package concrete.runner.sql

import com.typesafe.config.ConfigFactory
import slick.jdbc.GetResult
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object CompetMatrix extends App {
  lazy val systemConfig = ConfigFactory.load //defaults from src/resources
  lazy val DB = Database.forConfig("database", systemConfig)


  implicit val getProblemResult = GetResult(r => Problem(r.<<, r.<<, r.<<, r.<<))

  //var d = Array.ofDim[Int](configs.size, configs.size)

  // val ignoreNaN = true

  implicit val getExecutionResult = GetResult(r => Execution(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))

  implicit val getConfigResult = GetResult(r => Config(r.<<, r.<<, r.<<))
  val problemQuery = sql"""
        WITH Counts AS (
        SELECT "problemTag", count(*) FROM "ProblemTag" GROUP BY "problemTag")

        SELECT "problemId", display, "nbVars", Counts.count
        FROM "Problem" JOIN "ProblemTag" USING ("problemId") JOIN Counts USING ("problemTag")
        """.as[Problem]
  val executionQuery = sql"""

    SELECT "problemId", "configId", iteration, status, solution,
    cast(stat('solver.filter.revisions', "executionId") as real)/nullif(totalTime('{solver.searchCpu, solver.preproCpu, problemGenerator.genTime}', "executionId"), 0.0)
    -- totalTime('{solver.searchCpu, solver.preproCpu, runner.loadTime}', "executionId")/1e3
    FROM "Execution"
    WHERE iteration = 0 and status != 'started'
    -- WHERE "configId"  >= 0  AND "problemId" < 300 -- AND "problemId" < 976"""
    .as[Execution]

  val configQuery = sql"""SELECT "configId", config, description FROM "Config""""
    .as[Config]
  val groupedExecutions = for {
    problems <- DB.run(problemQuery)
    p = problems.map(q => q.problemId -> q).toMap
    executions <- DB.run(executionQuery)
  } yield {
    executions.flatMap { e =>
      p.get(e.problemId).map((_, e))
      //(problem, e)
    }
      .groupBy { case (prob, exec) => (prob, exec.iteration) }
      .values
    // .map(_.map(_._2))
    //.toSeq
    //.sortBy { case ((prob, iter), _) => (prob.problem, iter) }

  }
  val order = Array(42, 45, 41, 44)
  val matrixIndex = order.zipWithIndex.toMap
  val fut = for (
    pe <- groupedExecutions; cfgsSeq <- DB.run(configQuery);
    cfgs = cfgsSeq.map(c => c.configId -> c.desc).toMap
  ) yield {
    println(order.map(cfgs).mkString(" "))
    val matrix = Array.ofDim[Double](order.length, order.length)

    for {
      executions <- pe
      (p1, e1) <- executions
      (p2, e2) <- executions if e1 != e2
      e1c <- matrixIndex.get(e1.configId)
      e2c <- matrixIndex.get(e2.configId)
    } {
      require(p1 == p2)
      //      val c = e1.compareTo(e2)
      //      if (c > 0) {
      //        println((problem, e1, e2))
      //        matrix(e1c)(e2c) += 1
      //      }
      //      else if (c < 0) {
      //        matrix(e2c)(e1c) += 1
      //      }
      //      else {
      //        matrix(e1c)(e2c) +=   .5
      //        matrix(e2c)(e1c) +=   .5
      //      }
      //      if (Set(e1.configId, e2.configId) == Set(42, 45)) {
      //        println(s"${problem.problemId}")
      //        println(s"${e1.configId} ${e2.configId}: ${e1.scoreAgainst(e2)}")
      //      }
      println(s"${p1.problemId} ${p1.problem} ${p1.factor} ${e1.configId} ${e2.configId} ${e1.statistic} ${e2.statistic} ${e1.scoreAgainst(e2)}")
      matrix(e1c)(e2c) += e1.scoreAgainst(e2).get / p1.factor
    }


    println()
    matrix.foreach(r => println(r.map(d => f"$d%.2f").mkString(" ")))
    println()

    for (i <- 0 until order.length) {
      for (j <- 0 until order.length) {
        print(f"${matrix(i)(j) * 100 / 22}%2.0f" + " ")
      }
      println()
    }

    println(pe.size)


  }
  Await.result(fut, Duration.Inf)

  def doubleCompare(d1: Double, d2: Double, delta: Double) = {
    val ref = Math.max(Math.abs(d1), Math.abs(d2))
    if ((d1 - d2) / ref > delta) {
      1
    } else if ((d2 - d1) / ref > delta) {
      -1
    } else {
      0
    }
  }

  case class Problem(
                      problemId: Int,
                      problem: String,
                      nbVars: Int,
                      factor: Double)

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

    def compareTo(e: Execution): Int = {
      if (solved) {
        if (e.solved) {
          doubleCompare(
            statistic.get, e.statistic.get, .05)
        } else {
          1
        }
      } else if (e.solved) {
        -1
      } else {
        0
      }
    }

    def solved: Boolean = {
      status == "SAT1" || status == "UNSAT"
    }

    def scoreAgainst(e: Execution): Option[Double] = {
      for (s1 <- statistic; s2 <- e.statistic) yield {
        if (doubleCompare(s1, s2, .05) > 0) {
          1
        } else {
          0
        }

      }

    }


  }

}
