package concrete
import cspom.compiler.ProblemCompiler
import java.net.URL
import cspom.CSPOM
import cspfj.generator.ProblemGenerator
import java.security.MessageDigest
import java.math.BigInteger
import java.io.InputStream
import rb.RBGenerator
import rb.randomlists.RandomListGenerator.Structure
import rb.RBGenerator.Tightness
import java.io.StringReader
import java.io.BufferedReader
import java.io.ByteArrayInputStream

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
