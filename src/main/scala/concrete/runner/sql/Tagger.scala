package concrete.runner.sql
import slick.jdbc.StaticQuery.interpolation
import slick.driver.PostgresDriver.simple._
import slick.jdbc.GetResult
import java.net.URI

object Tagger extends App {

  case class Problem(
    problemId: Int,
    name: String,
    display: Option[String],
    tags: Seq[String])

  implicit val getProblemResult = GetResult(r =>
    Problem(r.<<, r.<<, r.<<, r.nextStringOption.map(_.split(",").toSeq).getOrElse(Seq())))

  Table.DB.withSession { implicit session =>

    val problems = sql"""
        SELECT "problemId", name, display, string_agg("problemTag", ',') as tags
        FROM "Problem" NATURAL LEFT JOIN "ProblemTag"
        GROUP BY "problemId", display, "nbVars", "nbCons"
        ORDER BY display
        """.as[Problem].list

    for (p <- problems) {
      val path = new URI(p.name).getPath.split('/')
      val name = path.last
      val cats = path.init

      if (p.display.isEmpty) {
        sqlu"""UPDATE "Problem" SET display = $name WHERE "problemId" = ${p.problemId}""".first
      }

      for (c <- cats if !p.tags.contains(c)) {
        sqlu"""INSERT INTO "ProblemTag" VALUES ($c, ${p.problemId})""".first
      }

    }
  }
}