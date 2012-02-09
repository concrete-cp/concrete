package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.constraint.Constraint
import cspfj.problem.BooleanDomain
import cspfj.problem.FALSE
import cspfj.problem.TRUE
import cspfj.problem.UNKNOWN
import cspfj.problem.Variable
import cspfj.UNSATException

final class ReifiedConstraint(
  controlVariable: Variable,
  val positiveConstraint: Constraint,
  val negativeConstraint: Constraint)
  extends AbstractConstraint(controlVariable +: positiveConstraint.scope) {

  require(positiveConstraint.scope forall scope.tail.contains)
  require(negativeConstraint.scope forall scope.tail.contains)

  val controlDomain = controlVariable.dom match {
    case bd: BooleanDomain => bd
    case _ => throw new IllegalArgumentException("Control variable must be boolean")
  }

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

  override def revise(reviseCount: Int) {
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
  }

  private def noReifyRevise(constraint: Constraint, reviseCount: Int) {
    val actualRevise = if (controlRemovals >= reviseCount) {
      -1;
    } else {
      reviseCount;
    }

    constraint.revise(actualRevise)
    if (constraint.isEntailed) {
      entail();
    }

  }

  def check = {
    tuple.copyToArray(positiveConstraint.tuple, 1, arity - 1)
    (value(0) == 1) == positiveConstraint.check
  }

  def getEvaluation =
    positiveConstraint.getEvaluation + negativeConstraint.getEvaluation

  override def toString = scope(0) + " == (" + positiveConstraint + ")";

  var controlRemovals = 0

  override def setRemovals(position: Int, value: Int) {
    if (position == 0) {
      controlRemovals = value;
    } else {
      positiveConstraint.setRemovals(position - 1, value);
      negativeConstraint.setRemovals(position - 1, value);
    }
  }

  override def fillRemovals(value: Int) {
    controlRemovals = value;
    positiveConstraint.fillRemovals(value);
    negativeConstraint.fillRemovals(value);
  }

}
