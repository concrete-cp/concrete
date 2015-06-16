package concrete.runner

import java.io.File
import java.net.URL
import java.nio.file.Files
import java.nio.file.StandardCopyOption

import scala.sys.process.stringSeqToProcess
import scala.util.Try

import concrete.generator.cspompatterns.XCSPPatterns
import cspom.CSPOM
import cspom.compiler.CSPOMCompiler
import cspom.xcsp.XCSPParser

object XCSPConcrete extends CSPOMRunner with App {

  var file: URL = _

  var declaredVariables: Seq[String] = _

  def loadCSPOM(args: List[String], opt: Map[Symbol, Any]) = {
    val List(fn) = args
    file = CSPOM.file2url(fn)
    for ((vars, cspom) <- loadCSPOMURL(file)) yield {
      declaredVariables = vars
      cspom
    }
  }

  def loadCSPOMURL(file: URL): Try[(Seq[String], CSPOM)] = {
    for ((cspom, data) <- CSPOM.load(file, XCSPParser)) yield {
      CSPOMCompiler.compile(cspom, XCSPPatterns())
      (data('variables).asInstanceOf[Seq[String]], cspom)
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

object TryWith {
  def apply[T <: AutoCloseable, Result](resGen: => T)(r: T => Result): Try[Result] = {

    Try(resGen).flatMap { closeable =>

      val res = Try(r(closeable))

      Try(closeable.close())
        .flatMap(_ => res)

    }
  }
}

class SolutionChecker(file: URL) {

  val temp: File = File.createTempFile("xcsp", new File(file.getFile).getName)
  temp.deleteOnExit()

  val r = TryWith(file.openStream) { in =>
    Files.copy(in, temp.toPath, StandardCopyOption.REPLACE_EXISTING)
  }

  //file.openStream()

  //Resource.fromURL(file).copyDataTo(tmpPath.outputStream())

  private val jar = classOf[SolutionChecker].getResource("Tools2008.jar").getFile

  private val command: Seq[String] =
    Seq("java", "-cp", jar, "abscon.instance.tools.SolutionChecker", temp.getPath)

  def checkSolution(solution: IndexedSeq[Int]): Option[String] = {
    val r = (command ++ solution.map(_.toString)).!!

    if (r.contains("solutionCost 0")) {
      None
    } else {
      Some(r)
    }

  }

}
