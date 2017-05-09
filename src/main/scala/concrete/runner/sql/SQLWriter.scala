/**
  * CSPFJ Competitor - CSP solver using the CSPFJ API for Java
  * Copyright (C) 2006 Julien VION
  *
  * This program is free software; you can redistribute it and/or
  * modify it under the terms of the GNU General Public
  * License as published by the Free Software Foundation; either
  * version 2 of the License, or (at your option) any later version.
  *
  * This library is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  * General Public License for more details.
  *
  * You should have received a copy of the GNU General Public
  * License along with this program; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
  */

package concrete
package runner
package sql

import java.io.{PrintWriter, StringWriter}
import java.net.InetAddress
import java.sql.{SQLException, Timestamp}
import java.util.Date

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import concrete.runner.sql.SQLWriter._
import cspom.StatisticsManager
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Try

object SQLWriter {

  lazy val systemConfig = ConfigFactory.load //defaults from src/resources
  //
  //  val systemConfig = Option(System.getProperty("concrete.config")) match {
  //    case Some(cfile) => ConfigFactory.parseFile(new File(cfile)).withFallback(baseConfig)
  //    case None => baseConfig
  //  }
  val problems = TableQuery[Problem]

  //val now = SimpleFunction.nullary[Timestamp]("now")
  val configs = TableQuery[Config]
  val executions = TableQuery[Execution]
  val problemTag = TableQuery[ProblemTag]
  val statistic = TableQuery[Statistic]

  def connection(createTables: Boolean): Database = {
    val db = Database.forConfig("database", systemConfig)

    if (createTables) {
      val setup = DBIO.seq(
        (problems.schema ++
          configs.schema ++
          executions.schema ++
          problemTag.schema ++
          statistic.schema).create,
        sqlu"""CREATE FUNCTION stat(field text, execution int) RETURNS text AS ${"$$"}
                 SELECT value FROM "Statistic" WHERE (name, "executionId") = (${"$"}1, ${"$"}2);
                ${"$$"} LANGUAGE sql""",

        sqlu"""CREATE FUNCTION totalTime(fields text[], executionId int) RETURNS real AS ${"$$"}
	               SELECT sum(cast(split_part(stat(unnest, executionId), ' ', 1) AS real)) FROM unnest(fields)
                ${"$$"} language sql;""")

      //          ,
      //        sqlu"""
      //          CREATE FUNCTION stat(field text, execution int) RETURNS text AS $$
      //            SELECT value FROM "Statistic" WHERE (name, "executionId") = ($1, $2);
      //          $$ LANGUAGE sql"""

      Await.ready(db.run(setup), Duration(10, SECONDS))
    }

    db

  }

  def now = new Timestamp(new Date().getTime)

  class Problem(tag: Tag)
    extends Table[(Int, String, Option[Int], Option[Int], Option[String], Option[String])](
      tag, "Problem") {
    def * = (problemId, name, nbVars, nbCons, display, nature)

    def problemId = column[Int]("problemId", O.PrimaryKey, O.AutoInc)

    def nbVars = column[Option[Int]]("nbVars")

    def nbCons = column[Option[Int]]("nbCons")

    def nature = column[Option[String]]("nature")

    def idxName = index("idxName", name, unique = true)

    def name = column[String]("name")

    def idxDisplay = index("idxDisplay", display, unique = true)

    def display = column[Option[String]]("display")
  }

  class Config(tag: Tag) extends Table[(Int, String, Option[String])](tag, "Config") {
    def * = (configId, config, description)

    def configId = column[Int]("configId", O.PrimaryKey, O.AutoInc)

    def config = column[String]("config")

    def description = column[Option[String]]("description")

    def idxMd5 = index("idxConfig", config, unique = true)
  }

  class Execution(tag: Tag) extends Table[(Int, Int, Int, Int, Timestamp, Option[Timestamp], Option[String], String, Option[String])](tag, "Execution") {
    def * = (executionId, configId, problemId, iteration, start, end, hostname, status, solution)

    def executionId = column[Int]("executionId", O.PrimaryKey, O.AutoInc)

    def configId = column[Int]("configId")

    def problemId = column[Int]("problemId")

    def iteration = column[Int]("iteration")

    def start = column[Timestamp]("start")

    def end = column[Option[Timestamp]]("end")

    def hostname = column[Option[String]]("hostname")

    def solution = column[Option[String]]("solution")

    def status = column[String]("status", O.Default("started"))

    def fkConfig = foreignKey("fkConfig", configId, configs)(_.configId, onDelete = ForeignKeyAction.Cascade)

    def fkProblem = foreignKey("fkProblem", problemId, problems)(_.problemId, onDelete = ForeignKeyAction.Cascade)

    def idxVCP = index("idxVCP", (configId, problemId, iteration), unique = true)
  }

  class ProblemTag(tag: Tag) extends Table[(String, Int)](tag, "ProblemTag") {
    def * = (problemTag, problemId)

    def fkProblem = foreignKey("fkProblem", problemId, problems)(_.problemId, onDelete = ForeignKeyAction.Cascade)

    def problemId = column[Int]("problemId")

    def pkPT = primaryKey("pkPT", (problemTag, problemId))

    def problemTag = column[String]("problemTag")
  }

  class Statistic(tag: Tag) extends Table[(String, Int, String)](tag, "Statistic") {
    def * = (name, executionId, value)

    def name = column[String]("name")

    def executionId = column[Int]("executionId")

    def value = column[String]("value")

    def fkExecution = foreignKey("fkExecution", executionId, executions)(_.executionId, onDelete = ForeignKeyAction.Cascade)

    def pk = primaryKey("pkS", (name, executionId))
  }

}

final class SQLWriter(params: ParameterManager, val stats: StatisticsManager)
  extends ConcreteWriter with LazyLogging {

  private lazy val db = Database.forConfig("database")

  private val initDB: Future[Any] = {
    if (params.contains("sql.createTables")) {
      db.run(
        (problems.schema ++
          configs.schema ++
          executions.schema ++
          problemTag.schema ++
          statistic.schema).create)
        .flatMap { _ =>
          db.run(
            sqlu"""CREATE OR REPLACE FUNCTION stat(field text, execution int) RETURNS text AS $$$$
                 SELECT CASE value 
                   WHEN 'None' THEN null 
                   ELSE value 
                 END 
                 FROM "Statistic" 
                 WHERE (name, "executionId") = ($$1, $$2);
                $$$$ LANGUAGE sql""")
        }
        .flatMap { _ =>
          db.run(
            sqlu"""CREATE OR REPLACE FUNCTION totaltime(fields text[], executionid integer) RETURNS real AS $$$$
	              SELECT sum(cast(split_part(stat(unnest, executionId), ' ', 1) as real)) 
	              FROM unnest(fields)
              $$$$ LANGUAGE sql""")
        }

    } else {
      Future.successful(Unit)
    }
  }

  private var executionId: Option[Int] = None

  private var configId: Future[Int] = _

  private var problemId: Future[Int] = _

  def parameters(params: ParameterManager) {

    configId = initDB
      .recover {
        case e: SQLException =>
          logger.warn("Table creation failed", e)

      }
      .flatMap { case _ => config(params) }

    configId.failed.foreach { pf =>
      logger.error("Failed to obtain configId", pf)
      throw pf
    }

    val it = params.getOrElse("iteration", 0)

    val ef = for (
      c <- configId;
      p <- problemId;
      e <- execution(p, c, it)
    ) yield e

    executionId =
      Some(Await.result(ef, Duration.Inf))
  }

  private def config(options: ParameterManager): Future[Int] = {
    val cfg = options.parameters
      .iterator
      .filter { case (k, v) => k != "iteration" }
      .map {
        case (k, Unit) => k
        case (k, v) => s"$k = $v"
      }.mkString(", ")

    val action = configs.filter(_.config === cfg).map(_.configId).result.headOption
    val result = db.run(action)

    result.flatMap {
      case None =>
        db.run(
          configs.map(c => (c.config)) returning configs.map(_.configId) += ((cfg)))
      case Some(c) => Future.successful(c)

    }

  }

  private def execution(p: Int, c: Int, it: Int) = {

    val executionId = db.run(
      executions.map(e =>
        (e.problemId, e.configId, e.start, e.hostname, e.iteration)) returning
        executions.map(_.executionId) += ((
        p, c, now,
        Some(InetAddress.getLocalHost.getHostName), it)))

    executionId.foreach { e =>
      print(s"Problem $p, config $c, iteration $it, execution $e")
    }

    executionId

  }

  def problem(name: String) {

    problemId = db.run(problems.filter(_.name === name).map(_.problemId).result.headOption)
      .flatMap {
        case Some(c) => Future.successful(c)
        case None => db.run(problems.map(p => p.name) returning problems.map(_.problemId) += name)
      }

    problemId.failed.foreach { pf =>
      logger.error("Failed to obtain problemId", pf)
      throw pf
    }

  }

  def solution(solution: String) {
    addSolution(solution, executionId.get)
  }

  def error(thrown: Throwable) {

    val errors = toString(thrown)

    System.err.println(errors)

    for (e <- executionId) {

      addSolution(errors, e)
      //        executions
      //          .filter(_.executionId === e)
      //          .map(_.solution)
      //          .update {
      //            Some(errors)
      //          }

    }
  }

  private def addSolution(solution: String, executionId: Int) = {
    val currentSolution = executions.filter(_.executionId === executionId).map(_.solution)
    //require(executionId.nonEmpty, "Problem description or parameters were not defined")
    val f = db.run {
      currentSolution.result.headOption
    }
      .flatMap { old =>
        val newSol = old.flatten.map(_ + "\n").getOrElse("") + solution
        db.run(currentSolution.update(Some(newSol)))
      }

    Await.ready(f, Duration.Inf)
  }

  private def toString(t: Throwable) = {
    val e = new StringWriter()
    t.printStackTrace(new PrintWriter(e))
    e.toString
  }

  def disconnect(status: Try[Result]) {
    try {
      for (e <- executionId) {

        val dbexec = for (dbe <- executions if dbe.executionId === e) yield dbe

        val result = status
          .map {
            case SatFinished => "SAT*"
            case SatUnfinished => "SAT1"
            case Unsat => "UNSAT"
          }
          .recover {
            case e =>
              def causes(e: Throwable): Seq[Throwable] = {
                Option(e).map(e => e +: causes(e.getCause)).getOrElse(Seq())
              }

              causes(e).mkString("\nCaused by: ")
          }
          .get

        val r0 = db.run {
          dbexec.map(e => (e.end, e.status)).update(
            (Some(SQLWriter.now), result))
        }

        val r = for ((key, value) <- stats.digest) yield {
          db.run {
            statistic += ((key, e, Option(value).map(_.toString).getOrElse("None")))
          }
        }

        Await.ready(Future.sequence(r0 +: r.toSeq), Duration.Inf)
      }
    } finally {
      db.close()
    }

  }

}
