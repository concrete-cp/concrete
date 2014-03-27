package concrete.runner.sql

import scala.slick.driver.PostgresDriver

trait MyPGDriver extends PostgresDriver {
  override def defaultSqlTypeName(tmd: JdbcType[_]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "TEXT"
    case _ => super.defaultSqlTypeName(tmd)
  }
}

object MyPGDriver extends MyPGDriver
