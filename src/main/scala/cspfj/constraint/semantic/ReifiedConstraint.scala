package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.constraint.Constraint
import cspfj.problem.BooleanDomain
import cspfj.problem.FALSE
import cspfj.problem.TRUE
import cspfj.problem.UNKNOWN
import cspfj.problem.Variable

final class ReifiedConstraint(
  controlVariable: Variable,
  val positiveConstraint: Constraint,
  val negativeConstraint: Constraint)
  extends AbstractConstraint(controlVariable +: positiveConstraint.scope) {

  require(positiveConstraint.scope forall scope.tail.contains)
  require(negativeConstraint.scope forall scope.tail.contains)
  require(controlVariable.dom.isInstanceOf[BooleanDomain], "Control variable must be boolean")

  val controlDomain = controlVariable.dom.asInstanceOf[BooleanDomain]

  //  private final class ReifiedRevisionHandler(val reifiedRevisator: RevisionHandler)
  //    extends RevisionHandler {
  //
  //    override def revised(constraint: Constraint, variable: Variable) {
  //      reifiedRevisator.revised(ReifiedConstraint.this, variable);
  //    }
  //
  //  }

  override def setLvl(l: Int) {
    super.setLvl(l)
    positiveConstraint.setLvl(l)
    negativeConstraint.setLvl(l)
  }

  override def restoreLvl(l: Int) {
    super.restoreLvl(l)
    positiveConstraint.restoreLvl(l)
    negativeConstraint.restoreLvl(l)
  }

  override def revise(reviseCount: Int): Boolean =
    controlDomain.status match {
      case UNKNOWN =>
        if (!positiveConstraint.isConsistent(reviseCount)) {
          controlDomain.setFalse();
          noReifyRevise(negativeConstraint, reviseCount)
        } else if (!negativeConstraint.isConsistent(reviseCount)) {
          controlDomain.setTrue();
          noReifyRevise(positiveConstraint, reviseCount)
        } else true

      case TRUE => noReifyRevise(positiveConstraint, reviseCount);
      case FALSE => noReifyRevise(negativeConstraint, reviseCount);

      case _ => throw new IllegalStateException

    }

  private def noReifyRevise(constraint: Constraint, reviseCount: Int) = {
    if (constraint.revise(reviseCount)) {
      if (constraint.isEntailed) {
        entail();
      }
      true;
    } else false;
  }

  def check = {
    tuple.copyToArray(positiveConstraint.tuple, 1, arity - 1)
    (value(0) == 1) == positiveConstraint.check
  }

  def getEvaluation =
    positiveConstraint.getEvaluation + negativeConstraint.getEvaluation

  override def toString = scope(0) + " == (" + positiveConstraint + ")";

}
