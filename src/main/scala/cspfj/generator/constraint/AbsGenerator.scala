package cspfj.generator.constraint;

import cspfj.constraint.semantic.Abs
import cspfj.problem.{ Variable, Problem, BitVectorDomain }
import cspom.constraint.CSPOMConstraint
import scala.collection.immutable.SortedSet

final class AbsGenerator(problem: Problem) extends AbstractGenerator(problem) {

  def generate(constraint: CSPOMConstraint) = {
    val Seq(result, v0) = constraint.scope map getSolverVariable

    if (Seq(result, v0) filter (_.getDomain == null) match {
      case Seq() => true
      case Seq(v) if v == v0 => {

        val values = result.getDomain.allValues

        v.setDomain(new BitVectorDomain(AbstractGenerator.makeDomain(values ++ values.map(-_))));
        true;
      }
      case Seq(v) if v == result => {
        val values = AbstractGenerator.makeDomain(result.getDomain.allValues.map(math.abs))
        v.setDomain(new BitVectorDomain(values));
        true;
      }
      case _ => false
    }) {
      addConstraint(new Abs(result, v0));
      true
    } else {
      false
    }

  }

}
