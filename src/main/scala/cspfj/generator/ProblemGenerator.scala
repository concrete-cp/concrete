package cspfj.generator;

import cspfj.generator.constraint.GeneratorManager
import cspfj.{ Domain, IntDomain, Problem }
import cspom.constraint.CSPOMConstraint
import cspom.variable.{ CSPOMDomain, BooleanDomain }
import cspom.CSPOM
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import cspom.variable.IntInterval
import cspfj.util.Loggable
import cspfj.UndefinedDomain

object ProblemGenerator extends Loggable {
  @throws(classOf[FailedGenerationException])
  def generate(cspom: CSPOM) = {

    // new ProblemCompiler(cspom).compile();

    val problem = new Problem();

    generateVariables(problem, cspom)

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

    for (v <- problem.variables if v.constraints.isEmpty) {
      problem.removeVariable(v)
    }

    problem;
  }

  def generateVariables(problem: Problem, cspom: CSPOM) {
    for (v <- cspom.variables) {
      logger.fine("sizeD " + v.domainOption.map(_.size).getOrElse("?"))
      problem.addVariable(v.name, generateDomain(v.domainOption));
    }
  }

  def generateDomain[T](cspomDomain: Option[CSPOMDomain[T]]): Domain = cspomDomain map {
    case bD: BooleanDomain =>
      if (bD.isConstant) new cspfj.BooleanDomain(bD.getBoolean)
      else new cspfj.BooleanDomain();

    case int: IntInterval => IntDomain(int.lb to int.ub)

    case ext: CSPOMDomain[Int] => IntDomain(ext.values: _*)

    case _ => throw new UnsupportedOperationException
  } getOrElse UndefinedDomain
}
