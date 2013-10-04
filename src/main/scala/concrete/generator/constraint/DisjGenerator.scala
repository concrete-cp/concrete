package concrete.generator.constraint;

import concrete.constraint.semantic.Disjunction
import concrete.{ Variable, Problem }
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.variable.CSPOMTrue
import cspom.variable.CSPOMFalse
import cspom.variable.BoolVariable

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
  private def generateReifiedOr(scope: Seq[Variable], constraint: CSPOMConstraint, result: Variable, args: Seq[Variable]) {
    require(!constraint.params.contains("revsign"),
      "Negative literals in reified disjunctions are currently not supported");

    val reverses = Seq(true).padTo(scope.size, false)

    addConstraint(new Disjunction(scope.toArray, reverses.toArray));

    for (v <- args) {
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
  private def generateReifiedAnd(scope: Seq[Variable], constraint: CSPOMConstraint, result: Variable, args: Seq[Variable]) {
    require(!constraint.params.contains("revsign"),
      "Negative literals in reified disjunctions are currently not supported");

    val reverses = Seq(false).padTo(scope.size, true)

    addConstraint(new Disjunction(scope.toArray, reverses.toArray));

    for (v <- args) {
      addConstraint(new Disjunction(Array(result, v), Array(true, false)));
    }
  }

  /**
   * Negation is converted to CNF :
   *
   * a = -b <=> (a v b) ^ (-a v -b)
   */
  private def generateNeg(result: Variable, arg: Variable) {

    addConstraint(new Disjunction(result, arg));
    addConstraint(new Disjunction(Array(result, arg), Array(true, true)));

  }

  override def gen(gC: CSPOMConstraint) = {
    require(gC.function == "or")

    val scope = gC.arguments map cspom2concreteVar

    scope foreach AbstractGenerator.booleanDomain
    val params: IndexedSeq[Boolean] = gC.params.get("revsign") match {
      case Some(p: Seq[Boolean]) => p.toIndexedSeq
      case None => IndexedSeq.fill(scope.size)(false)
      case p: Any => throw new IllegalArgumentException(s"Parameters for disjunction must be a sequence of boolean values, not '$p'")
    }

    addConstraint(new Disjunction(scope.toArray, params))
    true
  }

  override def genReified(fC: CSPOMConstraint, result: Variable) = {
    val scope = fC.arguments map cspom2concreteVar
    scope foreach AbstractGenerator.booleanDomain
    fC.function match {
      case 'or => generateReifiedOr(scope, fC, result, scope);
      case 'not if scope.size == 1 => val Seq(v) = scope; generateNeg(result, v);
      case 'and => generateReifiedAnd(scope, fC, result, scope);
      case _ => AbstractGenerator.fail("Unhandled constraint: " + fC);
    }
    true
  }

}
