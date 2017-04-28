package concrete.runner.sql

import com.typesafe.config.ConfigFactory
import slick.jdbc.PostgresProfile.api._
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.Future

object Mznc2SQL extends App {

  lazy val systemConfig = ConfigFactory.load //defaults from src/resources

  lazy val DB = Database.forConfig("database", systemConfig)

  val configs = Map(
    "Concrete-free" -> -1,
    "Choco-free" -> -2,
    "HaifaCSP-free" -> -3,
    "Chuffed-free" -> -4,
    "G12FD-free" -> -5,
    "Gecode-free" -> -6,
    "JaCoP-fd" -> -7,
    "LCG-Glucose-UC-free" -> -8,
    "LCG-Glucose-free" -> -9,
    "MZN/Cbc-free" -> -10,
    "MZN/CPLEX-free" -> -11,
    "MZN/Gurobi-free" -> -12,
    "MZN/SCIP-free" -> -13,
    "MinisatID-free" -> -14,
    "Mistral-free" -> -15,
    "OR-Tools-free" -> -16,
    "OscaR/CBLS-free" -> -17,
    "Picat CP-fd" -> -18,
    "Picat SAT-free" -> -19,
    "SICStus Prolog-fd" -> -20,
    "Yuck-free" -> -21,
    "iZplus-free" -> -22)

  val initF = Future.sequence {
    configs.map {
      case (config, id) =>
        DB.run(sqlu"""INSERT INTO "Config"
         SELECT $id, $config, $config WHERE $id NOT IN (
           SELECT "configId" FROM "Config")""")
    }
  }
    .flatMap(_ => DB.run(sqlu"""DELETE FROM "Execution" WHERE "configId" < 0"""))

  val processF = Source.fromFile("/home/vion/expes/mznc2016").getLines.grouped(23).flatMap {
    case problemLine :: results =>

      val pd = problemLine.split("\t").map(_.trim)

      val problem = s"(-|^)${pd(1)}.fzn.xz$$"

      val problemInfo = DB.run(sql"""SELECT "problemId", nature FROM "Problem" WHERE display ~ $problem""".as[(Int, String)])
        .map {
          case Seq(p) => p
          case _ => throw new IllegalArgumentException("Error with problem " + problem)
        }

      Seq.tabulate(3) { i => (problemInfo, i, results) }
  }
    .flatMap {
      case (problemInfo, iteration, results) =>

        for (resultLine <- results) yield {
          val solver +: status +: time +: sol +: _ = resultLine.split("\t").toList.map(_.trim)

          val configId = configs(solver)

          val sqlStatus = status match {
            case "S" => "SAT1"
            case "SC" => "SAT*"
            case "C" => "UNSAT"
            case "UNK" => "UNK"
          }

          val t = (if (status == "UNK") 1.2e6d else time.toDouble) / 1.2

          problemInfo
            .flatMap {
              case (p, nature) =>
                val solution = nature.split(" ").toSeq match {
                  case Seq("minimize", objective) if (status.contains("S")) => s"$objective = $sol;\n----------"
                  case Seq("maximize", objective) if (status.contains("S")) => s"$objective = $sol;\n----------"
                  case _ => ""
                }

                DB.run(
                  sql"""INSERT INTO "Execution" ("configId", "problemId", "iteration", "start", "status", "solution") 
                  VALUES ($configId, $p, $iteration, now(), $sqlStatus, $solution) 
                  RETURNING "executionId"""".as[Int])

            }.flatMap {
              case Seq(e) =>
                DB.run(
                  sqlu"""INSERT INTO "Statistic" VALUES ('solver.searchCpu', $e, $t), ('solver.preproCpu', $e, 0)""")
            }

        }
    }

  val s = Await.result(initF.flatMap(_ => Future.sequence(processF)), Duration.Inf)

  // require(problemId.length == 1, s"$problemId found for $problem")
  // println(problem + " : " + problemId.headOption)

}