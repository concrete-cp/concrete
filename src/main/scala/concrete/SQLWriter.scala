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

package concrete;

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.math.BigInteger
import java.net.URI
import java.net.URL
import java.security.MessageDigest
import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.SQLException

import cspfj.Solver
import cspfj.SolverResult
import cspfj.StatisticsManager
import cspom.CSPOM

object SQLWriter {
  def using[Closeable <: { def close(): Unit }, B](closeable: Closeable)(getB: Closeable => B): B =
    try {
      getB(closeable)
    } finally {
      closeable.close()
    }

  def bmap[T](test: => Boolean)(block: => T): List[T] =
    if (test) { block :: bmap(test)(block) }
    else { Nil }

  def queryUpdate(connection: Connection, sql: String) {
    using(connection.createStatement) { statement => statement.executeUpdate(sql) }
  }

  /** Executes the SQL and processes the result set using the specified function. */
  def query[B](connection: Connection, sql: String)(process: ResultSet => B): B =
    using(connection.createStatement) { statement =>
      using(statement.executeQuery(sql))(process)
    }

  /** Executes the SQL and uses the process function to convert each row into a T. */
  def queryEach[T](connection: Connection, sql: String)(process: ResultSet => T) =
    query(connection, sql) { results =>
      bmap(results.next())(process(results))
    }

  def connect(uri: URI) = {
    require(!uri.isOpaque, "Opaque connection URI : " + uri.toString)

    uri.getScheme match {
      case "mysql" =>
        Class.forName("com.mysql.jdbc.Driver");

      case "postgresql" =>
        Class.forName("org.postgresql.Driver")

      case _ =>
    }

    val userInfo = uri.getUserInfo match {
      case null => Array[String]()
      case info: String => info.split(":");
    }

    DriverManager.getConnection(
      "jdbc:%s://%s%s".format(uri.getScheme, uri.getHost, uri.getPath),
      (if (userInfo.length > 0) { userInfo(0) } else { null }),
      (if (userInfo.length > 1) { userInfo(1) } else { null }))

  }

}

final class SQLWriter(
  jdbcUri: URI,
  problemUrl: URL,
  params: String) extends ConcreteWriter {

  import SQLWriter._

  val connection = SQLWriter.connect(jdbcUri)

  createTables()

  val executionId = execution(
    problemId(problemUrl),
    config(params),
    Solver.VERSION + CSPOM.VERSION);

  //connection.close();

  private def controlTables(tables: String*) = {

    var foundTables: Set[String] = Set.empty
    using(connection.getMetaData.getTables(null, null, "%", null)) { results =>

      while (results.next()) foundTables += results.getString(3).toUpperCase
      tables.forall(foundTables)

    }

  }

  private def createTables() {
    if (!controlTables("EXECUTIONS", "CONFIGS", "PROBLEMS", "PROBLEMTAGS", "STATISTICS")) {

      using(connection.createStatement) { stmt =>

        val source = classOf[SQLWriter].getResource("concrete.sql")

        if (source == null) { sys.error("Could not find concrete.sql script") }

        val script = io.Source.fromURL(source).getLines.reduce(_ + _)

        script.split(';').foreach(stmt.addBatch)

        stmt.executeBatch()

      }
    }
  }
  def md5(istr: InputStream) = {
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

  private def problemId(problem: URL) = {
    val md5sum = md5(cspom.CSPOM.problemInputStream(problem))

    queryEach(connection, """
        SELECT problemId 
        FROM Problems 
        WHERE md5 = '%s'
        """.format(md5sum))(_.getInt(1)) match {
      case List(problemId) => problemId
      case _ => {
        queryEach(connection, """
    	      INSERT INTO Problems(name, md5) 
    	      VALUES ('%s', '%s') 
    	      RETURNING problemId
    	      """.format(problem.getFile, md5sum))(_.getInt(1)) match {
          case List(problemId) => problemId
          case _ =>
            throw new SQLException(
              "Could not retrieve generated key");
        }
      }
    }

  }

  private def config(options: String) = {

    val md5sum = md5(new ByteArrayInputStream(options.getBytes))

    val stmt = connection.createStatement();
    val rst = stmt.executeQuery("SELECT configId FROM configs WHERE md5='"
      + md5sum + "'");

    try {
      if (rst.next()) {
        rst.getInt(1);
      } else {
        val aiRst = stmt.executeQuery("INSERT INTO Configs (config, md5) VALUES (xml '<conf>"
          + options + "</conf>', '" + md5sum + "') RETURNING configId");

        try {
          if (aiRst.next()) {
            aiRst.getInt(1);
          } else {
            throw new SQLException(
              "Could not retrieve generated id");
          }
        } finally {
          aiRst.close();
        }
      }

    } finally {
      rst.close();
      stmt.close();
    }
  }

  def execution(problemId: Int, configId: Int, version: Int) = {
    val stmt = connection.createStatement();

    println("Problem " + problemId + ", config " + configId + ", version " + version)

    try {
      val aiRst = stmt.executeQuery("INSERT INTO Executions (problemId, configId, version, start) VALUES ("
        + problemId + ", " + configId + ", " + version + ", CURRENT_TIMESTAMP) RETURNING executionId");

      try {
        if (aiRst.next()) {
          aiRst.getInt(1);
        } else {
          throw new SQLException(
            "Could not retrieve generated id");
        }
      } finally {
        aiRst.close();
      }

    } finally {
      stmt.close();
    }
  }

  def solution(solution: SolverResult, problem: CSPOM) {

    val sol = outputFormat(solution, problem)

    val stmt = connection.createStatement();
    try {
      stmt.executeUpdate("UPDATE executions SET solution='"
        + sol + "' WHERE executionId="
        + executionId);
    } finally {
      stmt.close();
    }

  }

  def write(stats: StatisticsManager) {

    val stmt = connection
      .prepareStatement("INSERT INTO statistics(name, executionId, value) VALUES (?,?,?)");
    try {
      stmt.setInt(2, executionId)
      for ((key, value) <- stats.digest) {
        stmt.setString(1, key)
        stmt.setString(3, value.toString)
        stmt.executeUpdate()
      }
    } finally {
      stmt.close();
    }

  }

  def error(e: Throwable) {
    e.printStackTrace()
    val stmt = connection.createStatement();
    try {
      stmt.executeUpdate("UPDATE executions SET solution='"
        + e + "' WHERE executionId="
        + executionId);
    } finally {
      stmt.close();
    }
  }

  def disconnect() {
    val stmt = connection.createStatement()
    try {
      stmt.executeUpdate("UPDATE executions SET \"end\" = CURRENT_TIMESTAMP where executionId = " + executionId)
    } finally {
      stmt.close()
      connection.close()
    }
  }

}
