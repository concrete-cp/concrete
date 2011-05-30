package cspfj.generator.constraint;

import com.google.common.primitives.Ints
import cspfj.constraint.semantic.Mul
import cspfj.exception.FailedGenerationException
import cspfj.problem.BitVectorDomain
import cspfj.problem.Problem
import cspfj.problem.Variable
import cspom.constraint.CSPOMConstraint;
import scala.collection.immutable.SortedSet

final class MulGenerator(problem: Problem) extends AbstractGenerator(problem) {

  @throws(classOf[FailedGenerationException])
  def generate(constraint: CSPOMConstraint) = {
    val Seq(result, v0, v1) = constraint.scope map getSolverVariable

    if (Seq(result, v0, v1) filter (_.getDomain == null) match {
      case Seq() => true
      case Seq(v) if v == result => {
        val values = SortedSet[Int]() ++ (for (i <- v0.getDomain.allValues; j <- v1.getDomain.allValues) yield i * j)
        v.setDomain(new BitVectorDomain(values.toIndexedSeq))
        true
      }
      case Seq(v) if v == v0 => {
        v.setDomain(new BitVectorDomain(generateDomain(result, v1)))
        true
      }
      case Seq(v) if v == v1 => {
        v.setDomain(new BitVectorDomain(generateDomain(result, v0)))
        true
      }
      case _ => false

    }) {
      addConstraint(new Mul(result, v0, v1))
      true
    } else {
      false
    }

  }

  private def generateDomain(result: Variable, variable: Variable) = {

    (SortedSet[Int]() ++ (for (i <- result.getDomain.allValues; j <- result.getDomain.allValues if (i % j) == 0)
      yield i / j)).toIndexedSeq

  }

}
