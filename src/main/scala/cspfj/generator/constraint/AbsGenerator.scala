package cspfj.generator.constraint;

import cspfj.constraint.semantic.Abs
import cspfj.problem.{ Variable, Problem, BitVectorDomain }
import cspom.constraint.CSPOMConstraint
import scala.collection.immutable.SortedSet

final class AbsGenerator(problem: Problem) extends AbstractGenerator(problem) {

  def generate(constraint: CSPOMConstraint) = {
    val Seq(result, v0) = constraint.scope map cspom2cspfj

    if (Seq(result, v0) filter (_.domain == null) match {
      case Seq() => true
      case Seq(v) if v == v0 => {
        val values = result.domain.allValues
        v.domain = new BitVectorDomain(AbstractGenerator.makeDomain(values ++ values.map(-_)): _*);
        true;
      }
      case Seq(v) if v == result => {
        v.domain = new BitVectorDomain(AbstractGenerator.makeDomain(v0.domain.allValues.map(math.abs)): _*);
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
