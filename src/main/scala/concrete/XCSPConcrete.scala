package concrete

import cspom.compiler.ProblemCompiler
import java.net.URL
import cspom.CSPOM
import cspfj.generator.ProblemGenerator
import java.security.MessageDigest
import java.math.BigInteger
import java.io.InputStream

object XCSPConcrete extends Concrete with App {

  var cProblem: Option[CSPOM] = None

  def load(args: List[String]) = {
    val List(fileName) = args
    val file = new URL(fileName)
    cProblem = Some(CSPOM.load(file))
    (for (cp <- cProblem) yield {
      ProblemCompiler.compile(cp)
      val problem = ProblemGenerator.generate(cp)
      cp.closeRelations
      problem
    }).get

  }

  def description(args: List[String]) =
    args match {
      case List(fileName) =>
        (fileName, SQLWriter.md5(cspom.CSPOM.problemInputStream(new URL(fileName))))

      case _ => throw new IllegalArgumentException(args.toString)
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
