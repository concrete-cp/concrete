package concrete.runner.sql

import com.typesafe.config.ConfigFactory
import MyPGDriver.api._
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.util.Success

object Mznc2SQL extends App {

  lazy val systemConfig = ConfigFactory.load //defaults from src/resources

  lazy val DB = Database.forConfig("database", systemConfig)

  for (
    problemLine :: results <- Source.fromFile("/home/vion/expes/mznc2016").getLines.grouped(4)

  ) {
    val problem = problemLine.split("\t")(1)

    val problemInfo = DB.run(sql"""SELECT "problemId", nature FROM "Problem" WHERE display ~ $problem""".as[(Int, String)])
      .map { case Seq(p) => p }

    val r = for (resultLine <- results) yield {
      val solver +: status +: time +: sol +: _ = resultLine.split("\t").toList
      val configId = solver match {
        case "Concrete-free" => -1
        case "Choco-free" => -2
        case "HaifaCSP-free" => -3
      }

      val sqlStatus = status match {
        case "S" => "SAT1"
        case "SC" => "SAT*"
        case "C" => "UNSAT"
        case "UNK" => "UNK"
      }

      val t = if (status == "UNK") 1.2e6d else time.toDouble

      problemInfo.flatMap {
        case (p, nature) =>
          val solution = nature.split(" ").toSeq match {
            case Seq("minimize", objective) if (status.contains("S")) => s"$objective = $sol;\n----------"
            case Seq("maximize", objective) if (status.contains("S")) => s"$objective = $sol;\n----------"
            case _ => ""
          }

          DB.run(
            sql"""INSERT INTO "Execution" ("configId", "version", "problemId", "iteration", "start", "status", "solution") 
                  VALUES ($configId, 'MZNC2016', $p, 0, now(), $sqlStatus, $solution) RETURNING "executionId"""".as[Int])

      }.flatMap {
        case Seq(e) =>
          DB.run(
            sqlu"""INSERT INTO "Statistic" VALUES ('solver.searchCpu', $e, $t), ('solver.preproCpu', $e, 0)""")
      }

    }

    val s = Await.result(Future.sequence(r), Duration.Inf)

    // require(problemId.length == 1, s"$problemId found for $problem")
    // println(problem + " : " + problemId.headOption)

  }

}