package cspfj.generator.constraint;

import cspfj.constraint.semantic.Mul
import cspfj.generator.FailedGenerationException
import cspfj.{ Variable, Problem, IntDomain }
import cspom.constraint.CSPOMConstraint

final class MulGenerator(problem: Problem) extends AbstractGenerator(problem) {

  @throws(classOf[FailedGenerationException])
  def generate(constraint: CSPOMConstraint) = {
    val Seq(result, v0, v1) = constraint.scope map cspom2cspfj

    if (Seq(result, v0, v1) filter (_.dom == null) match {
      case Seq() => true
      case Seq(v) if v == result => {
        val values = AbstractGenerator.domainFrom(v0, v1, { _ * _ })
        v.dom = IntDomain(values: _*)
        true
      }
      case Seq(v) if v == v0 => {
        v.dom = IntDomain(generateDomain(result, v1): _*)
        true
      }
      case Seq(v) if v == v1 => {
        v.dom = IntDomain(generateDomain(result, v0): _*)
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
        i <- result.dom.values.toSeq
        j <- variable.dom.values.toSeq
        if (i % j == 0)
      } yield i / j)
  }

}
