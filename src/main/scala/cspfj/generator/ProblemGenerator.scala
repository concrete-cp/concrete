package cspfj.generator;

import cspfj.exception.FailedGenerationException
import cspfj.generator.constraint.GeneratorManager
import cspfj.problem.{BitVectorDomain, Problem}
import cspom.constraint.CSPOMConstraint
import cspom.variable.{CSPOMDomain, BooleanDomain}
import cspom.CSPOM
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object ProblemGenerator {
  @throws(classOf[FailedGenerationException])
  def generate(cspom: CSPOM) = {

    // new ProblemCompiler(cspom).compile();

    val problem = new Problem();

    for (v <- cspom.variables) {
      problem.addVariable(v.name, generateDomain(v.domain));
    }

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
    problem;
  }

  def generateDomain[T](cspomDomain: CSPOMDomain[T]) = cspomDomain match {
    case null => null
    case bD: BooleanDomain =>
      if (bD.isConstant) {
        new cspfj.problem.BooleanDomain(bD.getBoolean);
      } else {
        new cspfj.problem.BooleanDomain();
      }
    case _ =>
      new BitVectorDomain(cspomDomain.asInstanceOf[CSPOMDomain[Int]].values: _*);
  }
}
