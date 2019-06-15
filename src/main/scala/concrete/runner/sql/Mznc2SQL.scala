package concrete.runner.sql

import com.typesafe.config.ConfigFactory
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source

object Mznc2SQL extends App {

  lazy val systemConfig = ConfigFactory.load //defaults from src/resources

  lazy val DB = Database.forConfig("database", systemConfig)

  val configs = Map(
    "choco-fd" -> -1,
    "chuffed-fd" -> -2,
    "concrete-fd" -> -3,
    "gecode-fd" -> -4,
    "jacop-fd" -> -5,
    "or-tools-fd" -> -6,
    "sicstus-fd" -> -7,
  )

  val initF = Future.sequence {
    configs.map {
      case (config, id) =>
        DB.run(
          sqlu"""INSERT INTO "Config"
         SELECT $id, $config, $config WHERE $id NOT IN (
           SELECT "configId" FROM "Config")""")
    }
  }
  // .flatMap(_ => DB.run(sqlu"""DELETE FROM "Execution" WHERE "configId" < 0"""))

  val processF = Source.fromFile("/home/vion/expes/mznc-2018").getLines.grouped(1 + configs.size).flatMap {
    case problemLine :: results =>

      val pd = problemLine.split("\t").map(_.trim)

      val problem = s"(-|^)${pd(1)}.fzn.xz$$"

      val problemInfo = DB.run(sql"""SELECT "problemId", nature FROM "Problem" WHERE display ~ $problem""".as[(Int, String)])
        .map {
          case Seq(p) => p
          case e => throw new IllegalArgumentException("Error with problem " + problem + " : " + e)
        }

      Seq.tabulate(3) { i => (problemInfo, i, results) }
  }
    .flatMap {
      case (problemInfo, iteration, results) =>
        println(s"$problemInfo $iteration")
        for (resultLine <- results) yield {
          val solver :: status :: time :: sol :: _ = resultLine.split("\t").toList.map(_.trim)

          val configId = configs(solver)

          val sqlStatus = status match {
            case "S" => "SAT1"
            case "SC" => "SAT*"
            case "C" => "UNSAT"
            case e => e
          }

          val t: Option[Double] = if (sqlStatus.contains("SAT")) Some(time.toDouble / 1.2) else None

          println(s"$solver $sqlStatus ${t.getOrElse(Double.NaN)}")

          problemInfo
            .flatMap {
              case (p, nature) =>
                val solution = nature.split(" ").toSeq match {
                  case Seq("minimize", objective) if status.contains("S") => s"$objective = $sol;\n----------"
                  case Seq("maximize", objective) if status.contains("S") => s"$objective = $sol;\n----------"
                  case _ => ""
                }

                val query =
                  sql"""
                       |INSERT INTO "Execution" ("configId", "problemId", "iteration", "start", "status", "solution", version)
                       |VALUES ($configId, $p, $iteration, now(), $sqlStatus, $solution, 'mznc2017')
                       |ON CONFLICT ON CONSTRAINT "execution_configid_problemid_iteration_version_key" DO UPDATE
                       |  SET status = EXCLUDED.status, solution = EXCLUDED.solution
                       |RETURNING "executionId"
                  """.stripMargin


                DB.run(query.as[Int])

            }.flatMap {
            case Seq(e) =>
              t.map { t =>
                DB.run(
                  sql"""
                       |INSERT INTO "Statistic" VALUES ('solver.searchCpu', $e, $t), ('solver.preproCpu', $e, 0)
                       |ON CONFLICT ON CONSTRAINT "pkS" DO UPDATE
                       |  SET value = EXCLUDED.value
                        """.stripMargin.asUpdate)
              }
                .getOrElse {
                  Future.successful(())
                }
          }

        }
    }

  val s = Await.result(initF.flatMap(_ => Future.sequence(processF)), Duration.Inf)

  // require(problemId.length == 1, s"$problemId found for $problem")
  // println(problem + " : " + problemId.headOption)

}