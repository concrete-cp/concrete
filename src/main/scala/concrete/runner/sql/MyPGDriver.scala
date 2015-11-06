package concrete.runner.sql

import slick.ast.FieldSymbol
import slick.driver.PostgresDriver
import slick.jdbc.JdbcType

trait MyPGDriver extends PostgresDriver {
  override def defaultSqlTypeName(tmd: JdbcType[_], size: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "TEXT"
    case _ => super.defaultSqlTypeName(tmd, size)
  }
}

object MyPGDriver extends MyPGDriver
