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
        WITH Selection AS (
          SELECT DISTINCT "problemId" FROM "Problem" NATURAL JOIN "Execution"
          WHERE 0 <= totalTime('{solver.searchCpu, solver.preproCpu}', "executionId")
            OR status ~ 'Out'
        ),
        Counts AS (
          SELECT "problemTag", count(*) FROM "ProblemTag" NATURAL JOIN Selection GROUP BY "problemTag")

        SELECT "problemId", display, "nbVars", Counts.count
        FROM Selection JOIN "Problem" USING ("problemId") JOIN "ProblemTag" USING ("problemId") JOIN Counts USING ("problemTag")
        """.as[Problem]
  val executionQuery = sql"""

    SELECT "problemId", "configId", iteration, status, solution,
    cast(stat('solver.filter.revisions', "executionId") as real)/nullif(totalTime('{solver.searchCpu, solver.preproCpu}', "executionId"), 0.0)
    -- totalTime('{solver.searchCpu, solver.preproCpu, runner.loadTime}', "executionId")/1e3
    FROM "Execution"
    WHERE iteration <= 1 and status != 'started'"""
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
  val order = Array(41, 45, 42, 44)
  val matrixIndex = order.zipWithIndex.toMap
  val fut = for (
    pe <- groupedExecutions; cfgsSeq <- DB.run(configQuery);
    cfgs = cfgsSeq.map(c => c.configId -> c.desc).toMap
  ) yield {

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
      val score = e1.scoreAgainst(e2).get
      //if (e1.configId ==  45    && e2.configId==41 && score > 0 ) {
        println(s"${p1.problemId} ${p1.problem} ${p1.factor} ${e1.configId} ${e2.configId} ${e1.status.take(30)} ${e1.statistic} ${e2.status.take(30)} ${e2.statistic} $score")
      //}
      matrix(e1c)(e2c) += score  / p1.factor
    }

    println(order.map(cfgs).mkString(" "))

    println()
    matrix.foreach(r => println(r.map(d => f"$d%.2f").mkString(" ")))
    println()

    for (i <- 0 until order.length) {
      println(matrix(i).map(c => f"${c*50/10}%2.0f").mkString(" & ")+ " \\\\")
    }

    println(pe.size)


  }
  Await.result(fut, Duration.Inf)

  def doubleCompare(d1: Double, d2: Double, delta: Double) = {
    if ((d1 - d2) / d2 > delta) {
      1
    } else if ((d2 - d1) / d1 > delta) {
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
      if (memError) {
        Some(0)
      } else if (e.memError) {
        Some(1)
      } else for (s1 <- statistic; s2 <- e.statistic) yield {
        if ((s1-s2)/s2 > .10) {
          1
        } else {
          0
        }
      }


    }

    def memError: Boolean = status.contains("Out")


  }

}
