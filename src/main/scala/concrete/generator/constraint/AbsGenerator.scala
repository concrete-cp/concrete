package concrete.generator.constraint;

import concrete.constraint.semantic.Abs
import concrete.{ Variable, Problem, IntDomain }
import cspom.constraint.CSPOMConstraint
import scala.collection.immutable.SortedSet
import cspom.constraint.FunctionalConstraint

final class AbsGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generateFunctional(constraint: FunctionalConstraint) = {
    val Seq(result, v0) = constraint.scope map cspom2concrete

    val g = Seq(result, v0) filter (_.dom.undefined) match {
      case Seq() => true
      case Seq(`v0`) => {
        val values = result.dom.values.toSeq
        v0.dom = IntDomain(AbstractGenerator.makeDomain(values ++ values.map(-_)): _*);
        true;
      }
      case Seq(`result`) => {
        result.dom = IntDomain(AbstractGenerator.makeDomain(v0.dom.values.map(math.abs).toSeq): _*);
        true;
      }
      case _ => false
    }

    if (g) {
      addConstraint(new Abs(result, v0));
    }

    g

  }

}
