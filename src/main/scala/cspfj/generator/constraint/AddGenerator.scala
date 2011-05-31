package cspfj.generator.constraint;

import com.google.common.collect.ImmutableList
import com.google.common.collect.Iterables
import com.google.common.primitives.Ints
import cspfj.constraint.Constraint
import cspfj.constraint.semantic.Add
import cspfj.constraint.semantic.Eq
import cspfj.exception.FailedGenerationException
import cspfj.problem.BitVectorDomain
import cspfj.problem.Problem
import cspfj.problem.Variable
import cspom.constraint.CSPOMConstraint;
import scala.collection.immutable.SortedSet

final class AddGenerator(problem: Problem) extends AbstractGenerator(problem) {

  def generate(constraint: CSPOMConstraint) = {

    val Seq(result, v0, v1) = (constraint.description match {
      case "sub" => Seq(constraint.scope(1), constraint.scope(0), constraint.getVariable(2))
      case "add" => constraint.scope
      case _ => throw new IllegalArgumentException("Cannot handle " + constraint)
    }) map getSolverVariable

    if (Seq(result, v0, v1) filter { _.getDomain == null } match {
      case Seq() => true;
      case Seq(nullVariable) => {
        if (nullVariable == result) {
          val values = AbstractGenerator.makeDomain(for (i <- v0.getDomain.allValues; j <- v1.getDomain.allValues) yield i + j);

          result.setDomain(new BitVectorDomain(values));

        } else if (nullVariable == v0) {

          v0.setDomain(new BitVectorDomain(generateValues(result, v1)));

        } else if (nullVariable == v1) {

          v1.setDomain(new BitVectorDomain(generateValues(result, v0)));

        } else {

          throw new IllegalStateException();

        }
        true;
      }
      case _ => false;
    }) {

      addConstraint(
        if (result.getDomainSize == 1) {
          new Eq(-1, v0, result.getFirstValue(), v1);
        } else if (v0.getDomainSize == 1) {
          new Eq(1, v1, v0.getFirstValue, result);
        } else if (v1.getDomainSize == 1) {
          new Eq(1, v0, v1.getFirstValue, result);
        } else {
          new Add(result, v0, v1);
        });
      true;

    }
    false
  }

  def generateValues(result: Variable, variable: Variable) =
    AbstractGenerator.makeDomain(for (i <- result.getDomain.allValues; j <- result.getDomain.allValues) yield i - j)

}
