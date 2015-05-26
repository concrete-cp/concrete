package concrete.runner.sql

import slick.driver.PostgresDriver
import slick.jdbc.JdbcType
import slick.profile.RelationalProfile

trait MyPGDriver extends PostgresDriver {
  override def defaultSqlTypeName(tmd: JdbcType[_], size: Option[RelationalProfile.ColumnOption.Length]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "TEXT"
    case _ => super.defaultSqlTypeName(tmd, size)
  }
}

object MyPGDriver extends MyPGDriver
