package cspfj.generator.constraint;

import cspfj.constraint.semantic.Disjunction
import cspfj.{ Variable, Problem }
import cspom.constraint.{ GeneralConstraint, FunctionalConstraint, CSPOMConstraint }
import cspom.variable.CSPOMVariable

final class DisjGenerator(problem: Problem) extends AbstractGenerator(problem) {
  /**
   * Reified disjunction is converted to CNF :
   *
   * a = b v c v d...
   *
   * <=>
   *
   * (-a v b v c v d...) ^ (a v -b) ^ (a v -c) ^ (a v -d) ^ ...
   */
  private def generateReifiedOr(scope: Seq[Variable], constraint: FunctionalConstraint) {
    require(constraint.predicate.parameters == None,
      "Negative literals in reified disjunctions are currently not supported");

    val reverses = Seq(true).padTo(scope.size, false)

    addConstraint(new Disjunction(scope.toArray, reverses.toArray));

    val result = cspom2cspfj(constraint.result);

    for (v <- constraint.arguments map cspom2cspfj) {
      addConstraint(new Disjunction(Array(result, v), Array(false, true)));
    }
  }

  /**
   * Reified conjunction is converted to CNF :
   *
   * a = b ^ c ^ d...
   *
   * <=>
   *
   * (a v -b v -c v -d...) ^ (-a v b) ^ (-a v c) ^ (-a v d) ^ ...
   */
  private def generateReifiedAnd(scope: Seq[Variable], constraint: FunctionalConstraint) {
    require(constraint.predicate.parameters == None,
      "Negative literals in reified conjuctions are currently not supported");

    val reverses = Seq(false).padTo(scope.size, true)

    addConstraint(new Disjunction(scope.toArray, reverses.toArray));

    val result = cspom2cspfj(constraint.result);

    for (v <- constraint.arguments map cspom2cspfj) {
      addConstraint(new Disjunction(Array(result,
        v), Array(true, false)));
    }
  }

  /**
   * Negation is converted to CNF :
   *
   * a = -b <=> (a v b) ^ (-a v -b)
   */
  private def generateNeg(constraint: FunctionalConstraint) {

    val Seq(result, arg) = constraint.scope map cspom2cspfj

    addConstraint(new Disjunction(result, arg));
    addConstraint(new Disjunction(Array(result, arg), Array(true, true)));

  }

  override def generateGeneral(gC: GeneralConstraint) = {
    require(gC.description == "or")

    val scope = gC.scope map cspom2cspfj
    scope foreach AbstractGenerator.booleanDomain
    val params: IndexedSeq[Boolean] = gC.predicate.parameters match {
      case Some(p: Seq[Boolean]) => p.toIndexedSeq
      case None => IndexedSeq.fill(scope.size)(false)
      case _ => throw new IllegalArgumentException("Parameters for disjunction must be a sequence of boolean values")
    }

    addConstraint(new Disjunction(scope.toArray, params))
    true
  }

  override def generateFunctional(fC: FunctionalConstraint) = {
    val scope = fC.scope map cspom2cspfj
    scope foreach AbstractGenerator.booleanDomain
    fC.description match {
      case "or" => generateReifiedOr(scope, fC);
      case "not" => generateNeg(fC);
      case "and" => generateReifiedAnd(scope, fC);
      case _ =>
        throw new IllegalArgumentException("Unhandled constraint type for " + fC);
    }
    true
  }

}
