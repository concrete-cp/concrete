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
import java.net.URI
import java.security.MessageDigest
import cspom.StatisticsManager
import cspom.CSPOM
import java.security.InvalidParameterException
import scala.xml.NodeSeq
import concrete.runner.ConcreteRunner
import concrete.runner.ConcreteWriter
import concrete.Solver
import scala.slick.jdbc.meta.MTable
import java.sql.Timestamp
import scala.slick.model.PrimaryKey
import SQLWriter._
import java.net.InetAddress
import concrete.ParameterManager
import MyPGDriver.simple._
import concrete.Variable
import scala.slick.jdbc.StaticQuery.interpolation
import concrete.runner.RunnerStatus

object SQLWriter {

  def connection(uri: URI, createTables: Boolean) = {
    require(!uri.isOpaque, "Opaque connection URI : " + uri.toString)

    val driver = uri.getScheme match {
      // case "mysql" => "com.mysql.jdbc.Driver"

      case "postgresql" => "org.postgresql.Driver"

      case _ => throw new InvalidParameterException
    }

    val userInfo: IndexedSeq[String] = uri.getUserInfo match {
      case null => IndexedSeq[String]()
      case info: String => info.split(":");
    }

    val db = Database.forURL(s"jdbc:${uri.getScheme}://${uri.getHost}${uri.getPath}",
      user = userInfo.applyOrElse(0, { _: Int => null }),
      password = userInfo.applyOrElse(1, { _: Int => null }),
      driver = driver)

    if (createTables) {
      db.withSession { implicit session =>
        (problems.ddl ++
          configs.ddl ++
          executions.ddl ++
          problemTag.ddl ++
          statistic.ddl).create
      }
    }

    db

  }

  val now = SimpleFunction.nullary[Timestamp]("now")

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

  def findProblemByID = problems.findBy(_.problemId)
  def findProblemByName = problems.findBy(_.name)

  class Config(tag: Tag) extends Table[(Int, String, String)](tag, "Config") {
    def configId = column[Int]("configId", O.PrimaryKey, O.AutoInc)
    def config = column[String]("config")
    def md5 = column[String]("md5")

    def * = (configId, config, md5)

    def idxMd5 = index("idxMD5", md5, unique = true)
  }

  val configs = TableQuery[Config]

  def findConfigByMD5 = configs.findBy(_.md5)

  class Execution(tag: Tag) extends Table[(Int, String, Int, Int, Timestamp, Option[Timestamp], Option[String], String, Option[String])](tag, "Execution") {
    def executionId = column[Int]("executionId", O.PrimaryKey, O.AutoInc)
    def version = column[String]("version")
    def configId = column[Int]("configId")
    def problemId = column[Int]("problemId")
    def start = column[Timestamp]("start")
    def end = column[Option[Timestamp]]("end")
    def hostname = column[Option[String]]("hostname")
    def solution = column[Option[String]]("solution")
    def status = column[String]("status", O.Default("started"))

    def * = (executionId, version, configId, problemId, start, end, hostname, status, solution)

    def fkConfig = foreignKey("fkConfig", configId, configs)(_.configId, onDelete = ForeignKeyAction.Cascade)
    def fkProblem = foreignKey("fkProblem", problemId, problems)(_.problemId, onDelete = ForeignKeyAction.Cascade)
    def idxVCP = index("idxVCP", (version, configId, problemId), unique = true)
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
}

final class SQLWriter(jdbcUri: URI, params: ParameterManager) extends ConcreteWriter {

  val createTables: Boolean =
    params.getOrElse("sql.createTables", false)

  lazy val db = SQLWriter.connection(jdbcUri, createTables)

  //createTables()

  var executionId: Option[Int] = None

  private var configId: Option[Int] = None

  private var problemId: Option[Int] = None

  private def version: String = concrete.Info.version

  def parameters(params: NodeSeq) {
    configId = Some(config(params.toString))
    executionId = execution(problemId, configId, version)
  }

  def problem(name: String) {

    problemId = Some(db.withSession {
      implicit session =>

        findProblemByName(name).firstOption.map(_._1).getOrElse {
          problems.map(p => p.name) returning problems.map(_.problemId) += name
        }

    })
    executionId = execution(problemId, configId, version)
  }

  private def config(options: String) = {
    val istr = new ByteArrayInputStream(options.getBytes)
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
    val md5sum = "".padTo(32 - sum.length, '0') + sum

    db.withSession {
      implicit session =>

        configs.filter(_.md5 === md5sum).map(_.configId).firstOption.getOrElse {
          configs.map(c => (c.config, c.md5)) returning configs.map(_.configId) += ((options, md5sum))
        }
    }

  }

  private def execution(problemId: Option[Int], configId: Option[Int], version: String) = {
    for (p <- problemId; c <- configId) yield {
      print(s"Problem $p, config $c, version $version")

      val executionId = db.withSession { implicit session =>
        executions.map(e =>
          (e.problemId, e.configId, e.version, e.start, e.hostname)) returning
          executions.map(_.executionId) += ((
            p, c, version, SQLWriter.now.run,
            Some(InetAddress.getLocalHost.getHostName)))
      }

      println(s", execution $executionId")
      executionId
    }
  }

  def solution(solution: String) {
    require(executionId.nonEmpty, "Problem description or parameters were not defined")

    db.withSession { implicit session =>

      executions.filter(_.executionId === executionId.get).map(_.solution).update(Some(solution))

    }
  }

  def write(stats: StatisticsManager) {
    for (e <- executionId) {
      db.withSession { implicit session =>
        for ((key, value) <- stats.digest) {
          statistic += ((key, e, value.toString))
        }
      }
    }
  }

  def error(thrown: Throwable) {
    //println(e.toString)
    thrown.printStackTrace()
    for (e <- executionId) {
      db.withSession { implicit session =>
        executions.filter(_.executionId === e).map(_.solution).update(Some(thrown.toString))
      }
    }
  }

  def disconnect(status: RunnerStatus) {
    for (e <- executionId) {
      db.withSession { implicit session =>
        val dbexec = for (dbe <- executions if dbe.executionId === e) yield dbe
        dbexec.map(_.end).update(Some(SQLWriter.now.run))
        dbexec.map(_.status).update(status.toString())
      }
    }
  }

}
