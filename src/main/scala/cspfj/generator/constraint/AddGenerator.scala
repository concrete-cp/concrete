package cspfj.generator.constraint;

import cspfj.constraint.semantic.{ Eq, Add }
import cspfj.constraint.Constraint
import cspfj.problem.{ Variable, Problem, BitVectorDomain }
import cspom.constraint.CSPOMConstraint

final class AddGenerator(problem: Problem) extends AbstractGenerator(problem) {

  def generate(constraint: CSPOMConstraint) = {

    val Seq(result, v0, v1) = (constraint.description match {
      case "sub" => Seq(constraint.scope(1), constraint.scope(0), constraint.getVariable(2))
      case "add" => constraint.scope
      case _ => throw new IllegalArgumentException("Cannot handle " + constraint)
    }) map cspom2cspfj

    if (Seq(result, v0, v1) filter { _.domain == null } match {
      case Seq() => true;
      case Seq(nullVariable) => {
        if (nullVariable == result) {
          val values = AbstractGenerator.domainFrom(v0, v1, { _ + _ });

          result.domain = new BitVectorDomain(values: _*);

        } else if (nullVariable == v0) {

          v0.domain = new BitVectorDomain(generateValues(result, v1): _*);

        } else if (nullVariable == v1) {

          v1.domain = new BitVectorDomain(generateValues(result, v0): _*);

        } else {

          throw new IllegalStateException();

        }
        true;
      }
      case _ => false;
    }) {

      addConstraint(
        if (result.domain.size == 1) {
          new Eq(-1, v0, result.domain.firstValue, v1);
        } else if (v0.domain.size == 1) {
          new Eq(1, v1, v0.domain.firstValue, result);
        } else if (v1.domain.size == 1) {
          new Eq(1, v0, v1.domain.firstValue, result);
        } else {
          new Add(result, v0, v1);
        });
      true

    } else {
      false
    }
  }

  def generateValues(result: Variable, variable: Variable) =
    AbstractGenerator.domainFrom(result, variable, { _ - _ })

}