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

package concrete.runner.sql

import java.io.ByteArrayInputStream
import java.math.BigInteger
import java.net.InetAddress
import java.net.URI
import java.security.InvalidParameterException
import java.security.MessageDigest
import java.sql.Timestamp
import scala.xml.NodeSeq
import MyPGDriver.api._
import SQLWriter._
import concrete.ParameterManager
import concrete.runner.ConcreteWriter
import cspom.StatisticsManager
import slick.model.PrimaryKey
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import java.util.Date
import scala.util.Try
import com.typesafe.scalalogging.LazyLogging
import java.sql.SQLException
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import com.typesafe.config.ConfigFactory
import java.io.File
import scala.util.Failure
import scala.util.Success
import java.io.StringWriter
import java.io.PrintWriter

object SQLWriter {

  lazy val baseConfig = ConfigFactory.load //defaults from src/resources

  val systemConfig = Option(System.getProperty("concrete.config")) match {
    case Some(cfile) => ConfigFactory.parseFile(new File(cfile)).withFallback(baseConfig)
    case None        => baseConfig
  }

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

        sqlu"""CREATE FUNCTION totalTime(fields text[], executionId int) RETUNS real AS ${"$$"}
	               SELECT sum(cast(split_part(stat(unnest, executionId), ' ', 1) AS real)) FROM unnest(fields)
                ${"$$"} language sql;""")

      //          ,
      //        sqlu"""
      //          CREATE FUNCTION stat(field text, execution int) RETURNS text AS $$
      //            SELECT value FROM "Statistic" WHERE (name, "executionId") = ($1, $2);
      //          $$ LANGUAGE sql"""

      val setupFuture = db.run(setup)
    }

    db

  }

  //val now = SimpleFunction.nullary[Timestamp]("now")

  def now = new Timestamp(new Date().getTime)

  class Problem(tag: Tag)
      extends Table[(Int, String, Option[Int], Option[Int], Option[String])](
        tag, "Problem") {
    def problemId = column[Int]("problemId", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def nbVars = column[Option[Int]]("nbVars")
    def nbCons = column[Option[Int]]("nbCons")
    def display = column[Option[String]]("display")

    def * = (problemId, name, nbVars, nbCons, display)

    def idxName = index("idxName", name, unique = true)
    def idxDisplay = index("idxDisplay", display, unique = true)
  }

  val problems = TableQuery[Problem]

  class Config(tag: Tag) extends Table[(Int, String, String)](tag, "Config") {
    def configId = column[Int]("configId", O.PrimaryKey, O.AutoInc)
    def config = column[String]("config")
    def md5 = column[String]("md5")

    def * = (configId, config, md5)

    def idxMd5 = index("idxMD5", md5, unique = true)
  }

  val configs = TableQuery[Config]

  def findConfigByMD5 = configs.findBy(_.md5)

  class Execution(tag: Tag) extends Table[(Int, String, Int, Int, Int, Timestamp, Option[Timestamp], Option[String], String, Option[String])](tag, "Execution") {
    def executionId = column[Int]("executionId", O.PrimaryKey, O.AutoInc)
    def version = column[String]("version")
    def configId = column[Int]("configId")
    def problemId = column[Int]("problemId")
    def iteration = column[Int]("iteration")
    def start = column[Timestamp]("start")
    def end = column[Option[Timestamp]]("end")
    def hostname = column[Option[String]]("hostname")
    def solution = column[Option[String]]("solution")
    def status = column[String]("status", O.Default("started"))

    def * = (executionId, version, configId, problemId, iteration, start, end, hostname, status, solution)

    def fkConfig = foreignKey("fkConfig", configId, configs)(_.configId, onDelete = ForeignKeyAction.Cascade)
    def fkProblem = foreignKey("fkProblem", problemId, problems)(_.problemId, onDelete = ForeignKeyAction.Cascade)
    def idxVCP = index("idxVCP", (version, configId, problemId, iteration), unique = true)
  }

  val executions = TableQuery[Execution]

  class ProblemTag(tag: Tag) extends Table[(String, Int)](tag, "ProblemTag") {
    def problemTag = column[String]("problemTag")
    def problemId = column[Int]("problemId")

    def * = (problemTag, problemId)

    def fkProblem = foreignKey("fkProblem", problemId, problems)(_.problemId, onDelete = ForeignKeyAction.Cascade)
    def pkPT = primaryKey("pkPT", (problemTag, problemId))
  }

  val problemTag = TableQuery[ProblemTag]

  class Statistic(tag: Tag) extends Table[(String, Int, String)](tag, "Statistic") {
    def name = column[String]("name")
    def executionId = column[Int]("executionId")
    def value = column[String]("value")

    def * = (name, executionId, value)

    def fkExecution = foreignKey("fkExecution", executionId, executions)(_.executionId, onDelete = ForeignKeyAction.Cascade)
    def pk = primaryKey("pkS", (name, executionId))
  }

  val statistic = TableQuery[Statistic]

  def md5(data: String): String = {
    val istr = new ByteArrayInputStream(data.getBytes)
    val msgDigest = MessageDigest.getInstance("MD5");

    def createDigest(buffer: Array[Byte]) {
      val read = istr.read(buffer)
      if (read > 0) {
        msgDigest.update(buffer, 0, read)
        createDigest(buffer)
      }
    }

    createDigest(new Array[Byte](8192))

    val sum = new BigInteger(1, msgDigest.digest).toString(16);
    "".padTo(32 - sum.length, '0') + sum
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

  private def version: String = concrete.Info.version

  def parameters(params: ParameterManager) {

    configId = initDB
      .recover {
        case e: SQLException =>
          logger.warn("Table creation failed", e)

      }
      .flatMap { case _ => config(params) }

    configId.onFailure {
      case pf =>
        logger.error("Failed to obtain configId", pf)
        throw pf
    }

    val it = params.getOrElse("iteration", 0)

    val ef = for (
      c <- configId;
      p <- problemId;
      e <- execution(p, c, version, it)
    ) yield e

    executionId =
      Some(Await.result(ef, Duration.Inf))
  }

  def problem(name: String) {

    problemId = db.run(problems.filter(_.name === name).map(_.problemId).result.headOption)
      .flatMap {
        case Some(c) => Future.successful(c)
        case None    => db.run(problems.map(p => p.name) returning problems.map(_.problemId) += name)
      }

    problemId.onFailure {
      case pf =>
        logger.error("Failed to obtain problemId", pf)
        throw pf
    }

  }

  private def config(options: ParameterManager): Future[Int] = {
    val cfg = options.parameters
      .iterator
      .filter { case (k, v) => k != "iteration" }
      .map {
        case (k, Unit) => k
        case (k, v)    => s"$k = $v"
      }.mkString(", ")

    val md5sum = md5(cfg)
    val action = configs.filter(_.md5 === md5sum).map(_.configId).result.headOption
    val result = db.run(action)

    result.flatMap {
      case None =>
        db.run(
          configs.map(c => (c.config, c.md5)) returning configs.map(_.configId) += ((cfg, md5sum)))
      case Some(c) => Future.successful(c)

    }

  }

  private def execution(p: Int, c: Int, version: String, it: Int) = {

    val executionId = db.run(
      executions.map(e =>
        (e.problemId, e.configId, e.version, e.start, e.hostname)) returning
        executions.map(_.executionId) += ((
          p, c, version, now,
          Some(InetAddress.getLocalHost.getHostName))))

    executionId.onSuccess {
      case e =>

        print(s"Problem $p, config $c, version $version, execution $e")
    }

    executionId

  }

  def solution(solution: String) {
    val currentSolution = executions.filter(_.executionId === executionId.get).map(_.solution)
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

  def error(thrown: Throwable) {
    val errors = {
      val e = new StringWriter()
      thrown.printStackTrace(new PrintWriter(e))
      e.toString
    }
    System.err.println(errors)

    for (e <- executionId) {
      db.run {
        executions
          .filter(_.executionId === e)
          .map(_.solution)
          .update {
            Some(errors)
          }
      }
    }
  }

  def disconnect(status: Try[Boolean]) {
    try {
      for (e <- executionId) {

        val dbexec = for (dbe <- executions if dbe.executionId === e) yield dbe

        val result = status match {
          case Success(true)  => "SAT"
          case Success(false) => "UNSAT"
          case Failure(e) =>
            def causes(e: Throwable): Seq[Throwable] = {
              Option(e).map(e => e +: causes(e.getCause)).getOrElse(Seq())
            }
            causes(e).mkString("\nCaused by: ")
        }

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
