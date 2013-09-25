package concrete.generator.constraint;

import concrete.constraint.semantic.Mul
import concrete.generator.FailedGenerationException
import concrete.{ Variable, Problem, IntDomain }
import cspom.CSPOMConstraint

final class MulGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def genFunctional(constraint: CSPOMConstraint, r: C2Conc) = {
    val C2V(result) = r
    val Seq(v0, v1) = constraint.arguments map cspom2concreteVar

    if (Seq(result, v0, v1) filter (_.dom.undefined) match {
      case Seq() => true
      case Seq(`result`) => {
        val values = AbstractGenerator.domainFromVar(v0, v1, { _ * _ })
        result.dom = IntDomain(values: _*)
        true
      }
      case Seq(`v0`) => {
        v0.dom = IntDomain(generateDomain(result, v1): _*)
        true
      }
      case Seq(`v1`) => {
        v1.dom = IntDomain(generateDomain(result, v0): _*)
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
    AbstractGenerator.domainFromVarFlat(Seq(result, variable), {
      case Seq(i, j) => if (i % j == 0) Seq(i / j) else Seq()
    })
    //    AbstractGenerator.makeDomain(
    //      for {
    //        i <- result.dom.values.toSeq
    //        j <- variable.dom.values.toSeq
    //        if (i % j == 0)
    //      } yield i / j)
  }

}
