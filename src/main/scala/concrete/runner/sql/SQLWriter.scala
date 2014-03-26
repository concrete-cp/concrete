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
import scala.slick.driver.PostgresDriver.simple._
import scala.xml.NodeSeq
import concrete.runner.ConcreteRunner
import concrete.runner.ConcreteWriter
import concrete.Solver
import scala.slick.jdbc.meta.MTable
import java.sql.Timestamp
import scala.slick.model.PrimaryKey
import SQLWriter._
import java.net.InetAddress

object SQLWriter {

  def connection(uri: URI) = {
    require(!uri.isOpaque, "Opaque connection URI : " + uri.toString)

    val driver = uri.getScheme match {
      // case "mysql" => "com.mysql.jdbc.Driver"

      case "postgresql" => "org.postgresql.Driver"

      case _ => throw new InvalidParameterException
    }

    val userInfo = uri.getUserInfo match {
      case null => Array[String]()
      case info: String => info.split(":");
    }

    Database.forURL(s"jdbc:${uri.getScheme}://${uri.getHost}${uri.getPath}",
      user = (if (userInfo.length > 0) { userInfo(0) } else { null }),
      password = (if (userInfo.length > 1) { userInfo(1) } else { null }),
      driver = driver)

  }

  val now = SimpleFunction.nullary[Timestamp]("now")

  class Problem(tag: Tag) extends Table[(Int, String, Option[Int], Option[Int], String)](tag, "Problem") {
    def problemId = column[Int]("problemId", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def nbVars = column[Option[Int]]("nbVars")
    def nbCons = column[Option[Int]]("nbCons")
    def display = column[String]("display")

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

    def idxMd5 = index("idxMD5", md5, unique = true)
  }

  val configs = TableQuery[Config]

  class Execution(tag: Tag) extends Table[(Int, String, Int, Timestamp, Timestamp, String, String)](tag, "Execution") {
    def executionId = column[Int]("executionId", O.PrimaryKey, O.AutoInc)
    def version = column[String]("version")
    def configId = column[Int]("configId")
    def problemId = column[Int]("problemId")
    def start = column[Timestamp]("start")
    def end = column[Option[Timestamp]]("end")
    def hostname = column[Option[String]]("hostname")
    def solution = column[Option[String]]("solution")

    //def * = executionId ~ version ~ configId ~ problemId ~ start ~ end ~ hostname ~ solution

    def fkConfig = foreignKey("fkConfig", configId, configs)(_.configId, onDelete = ForeignKeyAction.Cascade)
    def fkProblem = foreignKey("fkProblem", problemId, problems)(_.problemId, onDelete = ForeignKeyAction.Cascade)
    def idxVCP = index("idxVCP", (version, configId, problemId), unique = true)
  }

  val executions = TableQuery[Execution]

  class ProblemTag(tag: Tag) extends Table[(String, Int)](tag, "ProblemTag") {
    def problemTag = column[String]("problemTag")
    def problemId = column[Int]("problemId")

    def fkProblem = foreignKey("fkProblem", problemId, problems)(_.problemId, onDelete = ForeignKeyAction.Cascade)
    def pk = primaryKey("pk", (problemTag, problemId))
  }

  val problemTag = TableQuery[ProblemTag]

  class Statistic(tag: Tag) extends Table[(String, Int, String)](tag, "Statistic") {
    def name = column[String]("name")
    def executionId = column[Int]("executionId")
    def value = column[String]("value")

    def fkExecution = foreignKey("fkExecution", executionId, executions)(_.executionId, onDelete = ForeignKeyAction.Cascade)
    def pk = primaryKey("pk", (name, executionId))
  }

  val statistic = TableQuery[Statistic]
}

final class SQLWriter(jdbcUri: URI) extends ConcreteWriter {

  lazy val db = SQLWriter.connection(jdbcUri)

  //createTables()

  var executionId: Int = -1

  private var configId: Int = -1

  private var problemId: Int = -1

  private def version: String = s"${Solver.VERSION}/${CSPOM.VERSION}"

  def parameters(params: NodeSeq) {
    configId = config(params.toString)
    if (problemId >= 0 && executionId < 0) {
      executionId = execution(problemId, configId, version)
    }
  }

  def problem(name: String) {

    problemId = db.withSession {
      implicit session =>

        findProblemByName(name).firstOption.map(_._1).getOrElse {
          problems.map(p => p.name) returning problems.map(_.problemId) += name
        }

    }
    if (configId >= 0 && executionId < 0) {
      executionId = execution(problemId, configId, version)
    }
  }

  //connection.close();

  //  private def createTables() {
  //    db.withDynSession {
  //      SQLWriter.problems.ddl
  //    }
  //  }
  //
  //  private def problemId(description: String) =
  //
  //    db.withSession {
  //      problem
  //      sql"""
  //        SELECT problemId
  //        FROM Problems
  //        WHERE name = $description""".as[Int].firstOption getOrElse
  //        sql"""
  //              INSERT INTO Problems(name)
  //              VALUES ($description)
  //              RETURNING problemId
  //              """.as[Int].first
  //    }

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

  private def execution(problemId: Int, configId: Int, version: String) = {
    print(s"Problem $problemId, config $configId, version $version")

    val executionId = db.withSession { implicit session =>
      executions.map(e =>
        (e.problemId, e.configId, e.version, e.start, e.hostname)) returning
        executions.map(_.executionId) += ((
          problemId, configId, version, SQLWriter.now.run,
          Some(InetAddress.getLocalHost.getHostName)))
    }
    //    catch {
    //      case e: SQLException => throw new IllegalArgumentException(e.getMessage())
    //    }
    println(s", execution $executionId")
    executionId
  }

  def outputFormat(solution: Option[Map[String, Any]], concrete: ConcreteRunner) =
    solution match {
      case Some(solution) => concrete.output(solution)
      case None => "UNSAT"
    }

  def solution(solution: Option[Map[String, Any]], concrete: ConcreteRunner) {
    require(executionId >= 0, "Problem description or parameters were not defined")
    val sol = outputFormat(solution, concrete)
    db.withSession { implicit session =>

      executions.filter(_.executionId === executionId).map(_.solution).update(Some(sol))

    }
  }

  def write(stats: StatisticsManager) {
    db.withSession { implicit session =>
      for ((key, value) <- stats.digest) {
        statistic += ((key, executionId, value.toString))
        //sqlu"INSERT INTO statistics(name, executionId, value) VALUES ($key, $executionId, ${value.toString})".execute
      }
    }
  }

  def error(e: Throwable) {
    //println(e.toString)
    e.printStackTrace()
    db.withSession { implicit session =>
      val ex = executions.findBy(_.executionId).apply(executionId).first
      //.map(_.solution) //.update(Some(e.toString))
    }
    //    db.withSession {
    //      sqlu"UPDATE executions SET solution=${e.toString} WHERE executionId=$executionId".execute
    //    }
  }

  def disconnect() {

    db.withSession { implicit session =>
      executions.filter(_.executionId === executionId).map(_.end).update(Some(SQLWriter.now.run))
      //sqlu"""UPDATE executions SET "end" = CURRENT_TIMESTAMP where executionId = $executionId""".execute
    }
  }

}
