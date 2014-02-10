package concrete.generator;

import concrete.generator.constraint.GeneratorManager
import concrete.{ Domain, IntDomain, Problem }
import cspom.CSPOMConstraint
import cspom.CSPOM
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import cspom.variable.IntInterval
import cspom.Loggable
import concrete.UndefinedDomain
import concrete.Variable
import cspom.variable.CSPOMVariable
import cspom.variable.BoolVariable
import cspom.variable.IntVariable
import cspom.variable.FreeVariable
import cspom.variable.IntSeq
import cspom.variable.FreeInt
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMExpression

object ProblemGenerator extends Loggable {
  @throws(classOf[FailedGenerationException])
  def generate(cspom: CSPOM) = {

    // new ProblemCompiler(cspom).compile();

    //val problem = new Problem();

    val variables = generateVariables(cspom)

    val problem = new Problem(variables.values.toList)

    //var firstFailed: Option[CSPOMConstraint] = None;

    @tailrec
    def processQueue(queue: Queue[CSPOMConstraint], firstFailed: Option[CSPOMConstraint]): Unit = if (queue.nonEmpty) {
      val (constraint, rest) = queue.dequeue

      GeneratorManager.generate(constraint, variables, problem) match {
        case Some(s) =>
          s.foreach(problem.addConstraint)

          processQueue(rest, None)

        case None =>
          if (firstFailed.exists(_ == constraint)) {
            throw new FailedGenerationException(
              "Could not generate the constraints " + queue);
          } else {
            processQueue(rest.enqueue(constraint), firstFailed orElse Some(constraint));
          }

      }
    }

    processQueue(Queue.empty ++ cspom.constraints, None)

    //    for (v <- problem.variables if v.constraints.isEmpty) {
    //      problem.removeVariable(v)
    //    }

    problem;
  }

  def generateVariables(cspom: CSPOM): Map[CSPOMVariable, Variable] = {

    var unnamed = 0;

    /**
     * Generates an unique variable name.
     *
     * @return An unique variable name.
     */
    def generate() = {
      val name = "_" + unnamed;
      unnamed += 1;
      name;
    }

    val named = cspom.namedExpressions.flatMap(
      ne => generateVariables(ne._1, ne._2))

    named ++ cspom.referencedExpressions.iterator.collect {
      case v: CSPOMVariable if !named.contains(v) =>
        v -> new Variable(generate(), generateDomain(v))
    }
  }

  def generateVariables(name: String, e: CSPOMExpression): Map[CSPOMVariable, Variable] =
    e match {
      case v: CSPOMVariable => Map(v -> new Variable(name, generateDomain(v)))
      case CSPOMSeq(vars, range, _) =>
        (vars zip range).flatMap {
          case (e, i) => generateVariables(s"$name[$i]", e)
        } toMap
      case _ => throw new UnsupportedOperationException(s"Cannot generate $e")
    }

  def generateDomain[T](cspomVar: CSPOMVariable): Domain = cspomVar match {
    case bD: BoolVariable =>
      new concrete.BooleanDomain();

    case v: IntVariable => v.domain match {
      case int: IntInterval => IntDomain(int)
      case IntSeq(seq) => IntDomain(seq: _*)
      case FreeInt => UndefinedDomain
    }

    case _: FreeVariable => UndefinedDomain
  }
}
