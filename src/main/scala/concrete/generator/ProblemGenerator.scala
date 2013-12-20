package concrete.generator;

import concrete.generator.constraint.GeneratorManager
import concrete.{ Domain, IntDomain, Problem }
import cspom.CSPOMConstraint
import cspom.CSPOM
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import cspom.variable.IntInterval
import concrete.util.Loggable
import concrete.UndefinedDomain
import concrete.Variable
import cspom.variable.CSPOMVariable
import cspom.variable.BoolVariable
import cspom.variable.IntVariable
import cspom.variable.FreeVariable
import cspom.variable.IntSeq
import cspom.variable.FreeInt

object ProblemGenerator extends Loggable {
  @throws(classOf[FailedGenerationException])
  def generate(cspom: CSPOM) = {

    // new ProblemCompiler(cspom).compile();

    //val problem = new Problem();

    val problem = new Problem(generateVariables(cspom))

    //var firstFailed: Option[CSPOMConstraint] = None;

    @tailrec
    def processQueue(queue: Queue[CSPOMConstraint], firstFailed: Option[CSPOMConstraint]): Unit = if (queue.nonEmpty) {
      val (constraint, rest) = queue.dequeue

      GeneratorManager.generate(constraint, problem) match {
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

  def generateVariables(cspom: CSPOM) = {
    cspom.variables.map { v =>
      new Variable(v.name, generateDomain(v));
    } toList
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
