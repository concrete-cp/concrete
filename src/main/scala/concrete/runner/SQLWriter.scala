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

package concrete.runner;

import java.io.ByteArrayInputStream
import java.math.BigInteger
import java.net.URI
import java.security.MessageDigest
import cspfj.Solver
import cspfj.StatisticsManager
import cspom.CSPOM
import java.security.InvalidParameterException
import scala.slick.session.Database
import scala.slick.jdbc.StaticQuery.interpolation
import Database.threadLocalSession
import java.net.InetAddress

object SQLWriter {

  def connection(uri: URI) = {
    require(!uri.isOpaque, "Opaque connection URI : " + uri.toString)

    val driver = uri.getScheme match {
      case "mysql" => "com.mysql.jdbc.Driver"

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

}

final class SQLWriter(
  jdbcUri: URI) extends ConcreteWriter {

  lazy val db = SQLWriter.connection(jdbcUri)

  //createTables()

  var executionId: Int = -1

  private var configId: Int = -1

  private var problemId: Int = -1

  private def version = Solver.VERSION + CSPOM.VERSION

  def parameters(params: String) {
    configId = config(params)
    if (problemId >= 0 && executionId < 0) {
      executionId = execution(problemId, configId, version)
    }
  }

  def problem(problem: String) {
    problemId = problemId(problem)
    if (configId >= 0 && executionId < 0) {
      executionId = execution(problemId, configId, version)
    }
  }

  //connection.close();

  private def controlTables(tables: String*) =
    db.withSession {
      val results = threadLocalSession.metaData.getTables(null, null, "%", null)

      try {
        var foundTables: Set[String] = Set.empty
        while (results.next()) foundTables += results.getString(3).toUpperCase
        tables.forall(foundTables)
      } finally {
        results.close()
      }

    }

  private def createTables() {
    if (!controlTables("EXECUTIONS", "CONFIGS", "PROBLEMS", "PROBLEMTAGS", "STATISTICS")) {

      val source = classOf[SQLWriter].getResource("concrete.sql")

      if (source == null) { sys.error("Could not find concrete.sql script") }

      val script = io.Source.fromURL(source).getLines.reduce(_ + _)

      db withSession {
        threadLocalSession.withStatement() {
          stmt =>
            script.split(';').foreach(stmt.addBatch)
            stmt.executeBatch()
        }
      }
    }
  }

  private def problemId(description: String) =
    db.withSession {
      sql"""
        SELECT problemId
        FROM Problems
        WHERE name = $description""".as[Int].firstOption getOrElse
        sql"""
              INSERT INTO Problems(name)
              VALUES ($description)
              RETURNING problemId
              """.as[Int].first
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

    //val md5sum = SQLWriter.md5(new ByteArrayInputStream(options.getBytes))
    db.withSession {
      sql"SELECT configId FROM configs WHERE md5=$md5sum".as[Int].firstOption getOrElse
        sql"""INSERT INTO Configs (config, md5) 
             VALUES (xml '<conf>#$options</conf>', $md5sum)
             RETURNING configId""".as[Int].first
    }
  }

  private def execution(problemId: Int, configId: Int, version: Int) = {
    print(s"Problem $problemId, config $configId, version $version")

    val executionId = db.withSession {
      sql"""INSERT INTO Executions (problemId, configId, version, start, hostname)
            VALUES ($problemId, $configId , $version, CURRENT_TIMESTAMP, ${InetAddress.getLocalHost.getHostName}) 
            RETURNING executionId""".as[Int].first
    }
    //    catch {
    //      case e: SQLException => throw new IllegalArgumentException(e.getMessage())
    //    }
    println(s", execution $executionId")
    executionId
  }

  def solution(solution: Option[Map[String, Int]], concrete: ConcreteRunner) {
    require(executionId >= 0, "Problem description or parameters were not defined")
    val sol = outputFormat(solution, concrete)
    db.withSession {
      sqlu"UPDATE Executions SET solution = $sol WHERE executionId = $executionId".execute
    }
  }

  def write(stats: StatisticsManager) {
    db.withSession {
      for ((key, value) <- stats.digest) {
        sqlu"INSERT INTO statistics(name, executionId, value) VALUES ($key, $executionId, ${value.toString})".execute
      }
    }
  }

  def error(e: Throwable) {
    //println(e.toString)
    e.printStackTrace()
    db.withSession {
      sqlu"UPDATE executions SET solution=${e.toString} WHERE executionId=$executionId".execute
    }
  }

  def disconnect() {
    db.withSession {
      sqlu"""UPDATE executions SET "end" = CURRENT_TIMESTAMP where executionId = $executionId""".execute
    }
  }

}