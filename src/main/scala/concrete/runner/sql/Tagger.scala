package concrete.runner.sql

import slick.jdbc.PostgresProfile.api._
import slick.jdbc.GetResult
import java.net.URI
import scala.concurrent.duration.Duration

object Tagger extends App {

  case class Problem(
    problemId: Int,
    name: String,
    display: Option[String],
    tags: Seq[String])

  implicit val getProblemResult = GetResult(r =>
    Problem(r.<<, r.<<, r.<<, r.nextStringOption.map(_.split(",").toSeq).getOrElse(Seq())))

  val DB = Database.forConfig("database")
  try {

    val run = DB.run(sql"""
        SELECT "problemId", name, display, string_agg("problemTag", ',') as tags
        FROM "Problem" LEFT JOIN "ProblemTag" USING ("problemId")
        WHERE name ~ '^instances/mznc'
        GROUP BY "problemId", display, "nbVars", "nbCons"

        -- HAVING string_agg("problemTag", ',') ~ 'xp-table'
        ORDER BY display
        """.as[Problem])

    //    run.onComplete {
    //      case Success(problems) =>
    //
    //        
    //
    //      case Failure(e) =>
    //        e.printStackTrace()
    //
    //      case _ => println("uh")
    //    }

    val problems = concurrent.Await.result(run, Duration.Inf)
    
    println(problems)

    val upd: Seq[DBIO[Int]] = problems.flatMap { p =>
      
      val path = new URI(p.name).getPath.split('/')
      println(path.toSeq)
      val cats = path.init

//      val display: Seq[DBIO[Int]] = if (p.display.isEmpty) {
//        Seq(sqlu"""UPDATE "Problem" SET display = $name WHERE "problemId" = ${p.problemId}""")
//      } else {
//        Seq()
//      }

      val tags: Seq[DBIO[Int]] = for (c <- cats if !p.tags.contains(c)) yield {
        sqlu"""INSERT INTO "ProblemTag" VALUES ($c, ${p.problemId})"""
        //null
      }

      //display ++: 
      tags

    }

    val go = DB.run(DBIO.sequence(upd))

    val r = concurrent.Await.result(go, Duration.Inf)
    
    println(r)
    //println(run.isCompleted)
  } finally {

    DB.close()
  }

}