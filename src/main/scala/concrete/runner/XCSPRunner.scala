package concrete.runner
import java.net.URL
import cspom.CSPOM
import abscon.instance.tools.InstanceParser
import java.util.StringTokenizer
import java.io.File
import abscon.instance.components.PConstraint
import abscon.instance.components.PVariable
import scala.collection.JavaConversions
import concrete.Variable
import cspom.variable.CSPOMVariable
import concrete.CSPOMSolver

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

  def applyParametersCSPOM(s: CSPOMSolver) = {}

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
  val instanceFileName = new File(file.toURI).getAbsolutePath
  require(new File(instanceFileName).exists(), "PROBLEM \t The file " + instanceFileName + " has not been found.")

  val parser = try {
    loadAndParseInstance(instanceFileName);
  } catch {
    case e: Exception =>
      throw new IllegalStateException("PROBLEM \t When loading and/or parsing file " + instanceFileName, e)
  }

  lazy val variablePositions = parser.getVariables().zipWithIndex.toMap

  private def loadAndParseInstance(instanceFileName: String) = {
    val parser = new InstanceParser();
    parser.loadInstance(instanceFileName);
    parser.parse(false);
    parser;
  }

  /**
   * Returns -1 if the solution is valid, the position of ther first invalid value otherwise
   */
  def isSolutionValid(solution: IndexedSeq[Int]): Int = {
    assert(parser.getVariables().length == solution.size)

    solution.indices.find {
      i => !parser.getVariables()(i).getDomain().contains(solution(i))
    } getOrElse (-1)

  }

  /**
   * Extract from the given solution the tuple of values that corresponds to the scope of the given constraint.
   */
  def buildTupleFor(constraint: PConstraint, solution: IndexedSeq[Int]) = {
    constraint.getScope().map(v => solution(variablePositions(v)))
  }

  def checkSolution(solution: IndexedSeq[Int]) = {
    require(parser.getVariables.length == solution.size,
      "PROBLEM \t The number of variables is " + parser.getVariables().length + " while the size of the solution is " + solution.length)

    val invalidPosition = isSolutionValid(solution);
    require(invalidPosition == -1, "ERROR \t The given solution is not valid as the " + invalidPosition + "th value of the solution is not present in the domain of the associated variable")

    val (list, costs) = JavaConversions.collectionAsScalaIterable(parser.getMapOfConstraints.values).map(c =>
      (c, c.computeCostOf(buildTupleFor(c, solution)))).filter(_._2 > 0).unzip

    if (list.nonEmpty) {
      Some("solutionCost " + costs.sum + ", listOfUnsatisfiedConstraints " + list)
    } else {
      None
    }
  }

}
