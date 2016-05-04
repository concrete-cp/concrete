package concrete.runner.sql

import slick.ast.FieldSymbol
import slick.driver.PostgresDriver
import slick.jdbc.JdbcType
import com.github.tminglei.slickpg.PgArraySupport

trait MyPGDriver extends PostgresDriver with PgArraySupport {
  override def defaultSqlTypeName(tmd: JdbcType[_], size: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "TEXT"
    case _                      => super.defaultSqlTypeName(tmd, size)
  }

  override val api = new API with ArrayImplicits with SimpleArrayPlainImplicits
  
}

object MyPGDriver extends MyPGDriver
