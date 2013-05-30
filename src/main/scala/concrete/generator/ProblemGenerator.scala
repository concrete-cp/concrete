package concrete.generator;

import concrete.generator.constraint.GeneratorManager
import concrete.{ Domain, IntDomain, Problem }
import cspom.constraint.CSPOMConstraint
import cspom.variable.{ CSPOMDomain, BooleanDomain }
import cspom.CSPOM
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import cspom.variable.IntInterval
import concrete.util.Loggable
import concrete.UndefinedDomain
import concrete.Variable

object ProblemGenerator extends Loggable {
  @throws(classOf[FailedGenerationException])
  def generate(cspom: CSPOM) = {

    // new ProblemCompiler(cspom).compile();

    //val problem = new Problem();

    val problem = new Problem(generateVariables(cspom))

    val gm = new GeneratorManager(problem);

    var firstFailed: Option[CSPOMConstraint] = None;

    @tailrec
    def processQueue(queue: Queue[CSPOMConstraint]): Unit = if (queue.nonEmpty) {
      val (constraint, rest) = queue.dequeue

      if (gm.generate(constraint)) {
        firstFailed = None;
        processQueue(rest)
      } else firstFailed match {
        case Some(c) if c == constraint =>
          throw new FailedGenerationException(
            "Could not generate the constraints " + queue);
        case Some(_) =>
          processQueue(rest.enqueue(constraint));
        case None =>
          firstFailed = Some(constraint);
          processQueue(rest.enqueue(constraint));
      }

    }

    processQueue(Queue.empty ++ cspom.constraints)

    //    for (v <- problem.variables if v.constraints.isEmpty) {
    //      problem.removeVariable(v)
    //    }

    problem;
  }

  def generateVariables(cspom: CSPOM) = {
    cspom.variables.map { v =>
      logger.fine("sizeD " + v.domainOption.map(_.size).getOrElse("?"))
      new Variable(v.name, generateDomain(v.domainOption));
    } toList
  }

  def generateDomain[T](cspomDomain: Option[CSPOMDomain[T]]): Domain = cspomDomain map {
    case bD: BooleanDomain =>
      if (bD.isConstant) new concrete.BooleanDomain(bD.getBoolean)
      else new concrete.BooleanDomain();

    case int: IntInterval => IntDomain(int.lb to int.ub)

    case ext: CSPOMDomain[Int] => IntDomain(ext.values: _*)

    case _ => throw new UnsupportedOperationException
  } getOrElse UndefinedDomain
}
