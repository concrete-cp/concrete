package concrete.runner

import java.net.URI
import java.net.URL

import scala.sys.process._

import concrete.generator.cspompatterns.XCSPPatterns
import cspom.CSPOM
import cspom.compiler.CSPOMCompiler
import cspom.xcsp.XCSPParser
import scalax.file.Path
import scalax.io.Resource

object XCSPConcrete extends CSPOMRunner with App {

  var file: URL = _

  var declaredVariables: Seq[String] = _

  def loadCSPOM(args: List[String], opt: Map[Symbol, Any]) = {
    val List(fn) = args
    file = CSPOM.file2url(fn)

    CSPOM.load(file, XCSPParser)
      .map {
        case (cspom, data) =>
          declaredVariables = data('variables).asInstanceOf[Seq[String]]
          cspom
      }
      .flatMap { cspom =>
        CSPOMCompiler.compile(cspom, Seq(XCSPPatterns()))
      }
  }

  def description(args: List[String]) =
    args match {
      case List(fileName) => fileName
      case _              => throw new IllegalArgumentException(args.toString)
    }

  def controlCSPOM(solution: Map[String, Any]) = {
    controlCSPOM(solution, declaredVariables, file)
  }

  def controlCSPOM(solution: Map[String, Any], variables: Iterable[String], file: URL) = {
    new SolutionChecker(file).checkSolution(variables.map(solution).map { case i: Int => i }.toIndexedSeq)
  }

  override def outputCSPOM(solution: Map[String, Any]): String = {
    declaredVariables.map(solution).mkString(" ")
  }

  run(args)

}

class SolutionChecker(file: URL) {

  private val tmpPath = Path.createTempFile(suffix = Path(file.getFile, '/').name)

  Resource.fromURL(file).copyDataTo(tmpPath.outputStream())

  private val jar = Path(classOf[SolutionChecker].getResource("Tools2008.jar").toURI).get.path

  private val command: Seq[String] =
    Seq("java", "-cp", jar, "abscon.instance.tools.SolutionChecker", tmpPath.path)

  def checkSolution(solution: IndexedSeq[Int]): Option[String] = {
    val r = (command ++ solution.map(_.toString)).!!

    if (r.contains("solutionCost 0")) {
      None
    } else {
      Some(r)
    }

  }

}
