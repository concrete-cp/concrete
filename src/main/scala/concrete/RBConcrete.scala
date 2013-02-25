package concrete

import java.io.ByteArrayInputStream
import cspfj.generator.ProblemGenerator
import cspom.CSPOM
import rb.RBGenerator
import rb.RBGenerator.Tightness
import rb.randomlists.RandomListGenerator.Structure
import java.net.URI
import scala.slick.session.Database
import scala.slick.jdbc.{ GetResult, StaticQuery => Q }
import Q.interpolation
import Database.threadLocalSession

object RBConcrete extends Concrete with App {

  var cProblem: Option[CSPOM] = None

  def load(args: List[String]) = {
    val Array(nbVariables, domainSize, arity, nbConstraints,
      tightness, seed) = args(0).split(":")

    val cp = new RBGenerator(nbVariables.toInt, domainSize.toInt, arity.toInt,
      nbConstraints.toInt, Tightness.PROPORTION, tightness.toDouble, seed.toInt,
      Structure.UNSTRUCTURED,
      Structure.UNSTRUCTURED, false, false).generate()
    cProblem = Some(cp)
    val problem = ProblemGenerator.generate(cp)
    cp.closeRelations
    problem
  }

  def description(args: List[String]) = {
    val name = "rb-" + args(0).split(":").mkString("-")
    (name, SQLWriter.md5(new ByteArrayInputStream(name.getBytes())))
  }

  def output(solution: Map[String, Int]) = {
    cProblem.get.variables.filter(!_.auxiliary).map(v =>
      solution.getOrElse(v.name, v.domain.values.head)).mkString(" ")
  }

  def control(solution: Map[String, Int]) = {
    cProblem.get.controlInt(solution) match {
      case s: Set[_] if s.isEmpty => None
      case s: Set[_] => Some(s.mkString(", "))
    }
  }

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
