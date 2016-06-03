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
import cspom.CSPOMGoal
import scala.util.Failure
import scala.util.Success
import org.apache.commons.compress.compressors.CompressorStreamFactory
import cspom.WithParam

object XCSPConcrete extends CSPOMRunner with App {

  var file: URL = _

  var declaredVariables: Seq[String] = _

  def loadCSPOM(args: List[String], opt: Map[Symbol, Any]): Try[CSPOM] = {
    val List(fn) = args
    file = CSPOM.file2url(fn)
    loadCSPOMURL(file).flatMap { cspom =>
      declaredVariables = cspom.goal.get match {
        case s @ WithParam(CSPOMGoal.Satisfy, _) =>
          s.getSeqParam("variables")
        case _ =>
          return Failure(new IllegalArgumentException("Variable sequence not available"))
      }
      Success(cspom)
    }
  }

  def loadCSPOMURL(file: URL): Try[CSPOM] = {
    CSPOM.load(file, XCSPParser)
      .flatMap { cspom =>
        CSPOMCompiler.compile(cspom, XCSPPatterns())
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
    new SolutionChecker(file).checkSolution(
      variables.map(solution).map { case i: Int => i }.toIndexedSeq)
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

  val temp: File = File.createTempFile("xcsp", new File(file.getFile).getName + ".xml")
  temp.deleteOnExit()

  TryWith {
    val in = file.openStream
    new CompressorStreamFactory().createCompressorInputStream(in)
  } { in =>
    Files.copy(in, temp.toPath, StandardCopyOption.REPLACE_EXISTING)
  }
    .get

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
