package concrete.runner

import java.io.File
import java.net.URL

import scala.sys.process._

import cspom.CSPOM

object XCSPConcrete extends CSPOMRunner with App {

  var file: URL = _

  var declaredVariables: Seq[String] = _

  def loadCSPOM(args: List[String]) = {
    val List(fn) = args
    file = new URL(fn)
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

  run(args)

}

class SolutionChecker(file: URL) {
  //println(file)
  private val instanceFileName = new File(file.toURI).getAbsolutePath
  require(new File(instanceFileName).exists(), "PROBLEM \t The file " + instanceFileName + " has not been found.")

  private val jar = new File(classOf[SolutionChecker].getResource("Tools2008.jar").toURI()).getAbsolutePath

  private val command = Seq("java", "-cp", jar, "abscon.instance.tools.SolutionChecker", instanceFileName)

  def checkSolution(solution: IndexedSeq[Int]): Option[String] = {
    val r = (command ++ solution.map(_.toString)) !!

    if (r.contains("solutionCost 0")) {
      None
    } else {
      Some(r)
    }

  }

}
