package cspfj.generator.constraint;

import cspfj.constraint.semantic.Mul
import cspfj.exception.FailedGenerationException
import cspfj.problem.{ Variable, Problem, BitVectorDomain }
import cspom.constraint.CSPOMConstraint

final class MulGenerator(problem: Problem) extends AbstractGenerator(problem) {

  @throws(classOf[FailedGenerationException])
  def generate(constraint: CSPOMConstraint) = {
    val Seq(result, v0, v1) = constraint.scope map cspom2cspfj

    if (Seq(result, v0, v1) filter (_.domain == null) match {
      case Seq() => true
      case Seq(v) if v == result => {
        val values = AbstractGenerator.domainFrom(v0, v1, { _ * _ })
        v.domain = new BitVectorDomain(values: _*)
        true
      }
      case Seq(v) if v == v0 => {
        v.domain = new BitVectorDomain(generateDomain(result, v1): _*)
        true
      }
      case Seq(v) if v == v1 => {
        v.domain = new BitVectorDomain(generateDomain(result, v0): _*)
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
    AbstractGenerator.makeDomain(
      for {
        i <- result.domain.allValues
        j <- variable.domain.allValues
        if (i % j == 0)
      } yield i / j)
  }

}
