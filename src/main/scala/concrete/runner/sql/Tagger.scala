package concrete.runner.sql

import slick.driver.PostgresDriver.api._
import slick.jdbc.GetResult
import java.net.URI
import scala.concurrent.ExecutionContext.Implicits.global

object Tagger extends App {

  case class Problem(
    problemId: Int,
    name: String,
    display: Option[String],
    tags: Seq[String])

  implicit val getProblemResult = GetResult(r =>
    Problem(r.<<, r.<<, r.<<, r.nextStringOption.map(_.split(",").toSeq).getOrElse(Seq())))

  val DB = Table.DB
  try {

    DB.run(sql"""
        SELECT "problemId", name, display, string_agg("problemTag", ',') as tags
        FROM "Problem" NATURAL LEFT JOIN "ProblemTag"
        GROUP BY "problemId", display, "nbVars", "nbCons"
        ORDER BY display
        """.as[Problem])
      .onSuccess {
        case problems =>

          val upd: Seq[DBIO[Int]] = problems.flatMap { p =>
            val path = new URI(p.name).getPath.split('/')
            val name = path.last
            val cats = path.init

            val display: Seq[DBIO[Int]] = if (p.display.isEmpty) {
              Seq(sqlu"""UPDATE "Problem" SET display = $name WHERE "problemId" = ${p.problemId}""")
            } else {
              Seq()
            }

            val tags: Seq[DBIO[Int]] = for (c <- cats if !p.tags.contains(c)) yield {
              sqlu"""INSERT INTO "ProblemTag" VALUES ($c, ${p.problemId})"""
            }

            display ++: tags

          }
          
          DB.run(DBIO.sequence(upd))

      }
  } finally {
    DB.close()
  }

}