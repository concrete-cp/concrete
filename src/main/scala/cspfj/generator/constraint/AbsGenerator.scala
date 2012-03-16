package cspfj.generator.constraint;

import cspfj.constraint.semantic.Abs
import cspfj.problem.{ Variable, Problem, IntDomain }
import cspom.constraint.CSPOMConstraint
import scala.collection.immutable.SortedSet

final class AbsGenerator(problem: Problem) extends AbstractGenerator(problem) {

  def generate(constraint: CSPOMConstraint) = {
    val Seq(result, v0) = constraint.scope map cspom2cspfj

    if (Seq(result, v0) filter (_.dom == null) match {
      case Seq() => true
      case Seq(v) if v == v0 => {
        val values = result.dom.values.toSeq
        v.dom = IntDomain(AbstractGenerator.makeDomain(values ++ values.map(-_)): _*);
        true;
      }
      case Seq(v) if v == result => {
        v.dom = IntDomain(AbstractGenerator.makeDomain(v0.dom.values.map(math.abs).toSeq): _*);
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
