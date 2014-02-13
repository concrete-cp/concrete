package concrete.runner
import rb.RBGenerator
import rb.RBGenerator.Tightness
import rb.randomlists.Structure
import java.net.URI
import scala.slick.session.Database
import scala.slick.jdbc.StaticQuery.interpolation
import Database.threadLocalSession

object RBConcrete extends ConcreteRunner with App {

  override def loadCSPOM(args: List[String]) = {
    val Array(nbVariables, domainSize, arity, nbConstraints,
      tightness, seed) = args(0).split(":")

    new RBGenerator(nbVariables.toInt, domainSize.toInt, arity.toInt,
      nbConstraints.toInt, Tightness.PROPORTION, tightness.toDouble, seed.toInt,
      Structure.UNSTRUCTURED,
      Structure.UNSTRUCTURED, false, false).generate()

  }

  def description(args: List[String]) = {
    "rb-" + args(0).split(":").mkString("-")
  }

  def control(solution: Map[String, Any]) = ???

  run(args)

}

object NameRB extends App {
  SQLWriter.connection(new URI("postgresql://concrete:concrete@precision-vion")).withSession {
    val f = io.Source.fromFile(args(0))
    for (line <- f.getLines) {
      val Array(n, d, k, e, l) = line.split(":")
      val lp = f"${100 * l.toDouble}%.0f"
      for (s <- 0 until 10) {
        println(s"rb-$n-$d-$k-$e-$l-$s")
        sqlu"""
          UPDATE Problems 
          SET display=${s"rb-$n-$d-$k-$e-$lp-$s"}, nbVars=${n.toInt}, nbCons=${e.toInt}
          WHERE name=${s"rb-$n-$d-$k-$e-$l-$s"}""".execute
      }

      sqlu"""
        INSERT INTO ProblemTags 
        SELECT ${s"rb-$n-$d-$k-$e-$lp"}, problemId 
        FROM Problems NATURAL LEFT JOIN ProblemTags
        WHERE name~${s"^rb-$n-$d-$k-$e-$l-"}
          AND tag IS NULL""".execute
    }

  }
}
