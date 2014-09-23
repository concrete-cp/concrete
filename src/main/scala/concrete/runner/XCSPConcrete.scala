package concrete.runner

import java.net.URL
import scala.sys.process._
import cspom.CSPOM
import scalax.io.Resource
import java.net.URI
import scalax.file.Path

object XCSPConcrete extends CSPOMRunner with App {

  var file: URL = _

  var declaredVariables: Seq[String] = _

  def loadCSPOM(args: List[String]) = {
    val List(fn) = args
    val f = new URI(fn)

    file = if (f.getScheme() == null) {
      new URL("file:" + f)
    } else {
      //println(f.getScheme())
      f.toURL
    }

    val cspom = CSPOM.load(file)
    declaredVariables = cspom._2('variables).asInstanceOf[Seq[String]]
    cspom._1
  }

  def description(args: List[String]) =
    args match {
      case List(fileName) => fileName
      case _ => throw new IllegalArgumentException(args.toString)
    }

  def controlCSPOM(solution: Map[String, Any]) = {
    controlCSPOM(solution, declaredVariables, file)
  }

  def controlCSPOM(solution: Map[String, Any], variables: Iterable[String], file: URL) = {
    new SolutionChecker(file).checkSolution(variables.map(v => solution(v).asInstanceOf[Int]).toIndexedSeq)
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
