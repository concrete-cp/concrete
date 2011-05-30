package cspfj.generator.constraint;

import cspfj.constraint.semantic.Abs
import cspfj.problem.{Variable, Problem, BitVectorDomain}
import cspom.constraint.CSPOMConstraint
import scala.collection.immutable.SortedSet

final class AbsGenerator(problem: Problem) extends AbstractGenerator(problem) {

  def handles = Seq("abs")
  
  def generate(constraint: CSPOMConstraint) = {
    val Seq(result, v0) = constraint.scope map getSolverVariable

    if (Seq(result, v0) filter (_.getDomain == null) match {
      case Seq() => false
      case Seq(v0) => {
        val values = (SortedSet[Int]() ++ result.getDomain.allValues.iterator.map(i => List(i, -i)).flatten).toSeq
        v0.setDomain(new BitVectorDomain(values: _*));
        true;
      }
      case Seq(result) => {
        val values = (SortedSet[Int]() ++ result.getDomain.allValues.iterator.map(math.abs)).toSeq
        result.setDomain(new BitVectorDomain(values: _*));
        true;
      }
      case _ => true
    }) {
      addConstraint(new Abs(result, v0));
      true
    } else { false }

  }

}
