package concrete.generator.constraint;

import concrete.constraint.semantic.{ Eq, Add }
import concrete.constraint.Constraint
import concrete.{ Variable, Problem, IntDomain }
import cspom.constraint.CSPOMConstraint
import cspom.constraint.FunctionalConstraint

final class AddGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generateFunctional(constraint: FunctionalConstraint) = {

    val Seq(result, v0, v1) = (constraint.description match {
      case "sub" => Seq(constraint.arguments(0), constraint.result, constraint.arguments(1))
      case "add" => constraint.result +: constraint.arguments
      case _ => throw new IllegalArgumentException("Cannot handle " + constraint)
    }) map cspom2concrete

    if (Seq(result, v0, v1) filter { _.dom.undefined } match {
      case Seq() => true;
      case Seq(`result`) =>
        val values = AbstractGenerator.domainFrom(v0, v1, { _ + _ });
        result.dom = IntDomain(values: _*);
        true
      case Seq(`v0`) =>
        v0.dom = IntDomain(generateValues(result, v1): _*);
        true
      case Seq(`v1`) =>
        v1.dom = IntDomain(generateValues(result, v0): _*);
        true
      case _ => false;
    }) {

      addConstraint(
        if (result.dom.size == 1) {
          new Eq(true, v0, result.dom.firstValue, v1);
        } else if (v0.dom.size == 1) {
          new Eq(false, v1, v0.dom.firstValue, result);
        } else if (v1.dom.size == 1) {
          new Eq(false, v0, v1.dom.firstValue, result);
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
