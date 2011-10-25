package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.constraint.Constraint
import cspfj.filter.RevisionHandler
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

  var usualRevisator: RevisionHandler = null
  var usualReifiedRevisator: RevisionHandler = null

  private final class ReifiedRevisionHandler(val reifiedRevisator: RevisionHandler)
    extends RevisionHandler {

    override def revised(constraint: Constraint, variable: Variable) {
      reifiedRevisator.revised(ReifiedConstraint.this, variable);
    }

  }

  override def level_=(l: Int) {
    super.level = l
    positiveConstraint.level = l
    negativeConstraint.level = l
  }

  override def revise(revisator: RevisionHandler, reviseCount: Int): Boolean =
    controlDomain.status match {
      case UNKNOWN =>
        if (!positiveConstraint.isConsistent(reviseCount)) {
          controlDomain.setFalse();
          if (noReifyRevise(negativeConstraint, revisator, reviseCount)) {
            revisator.revised(this, scope(0));
            true
          } else false
        } else if (!negativeConstraint.isConsistent(reviseCount)) {
          controlDomain.setTrue();
          if (noReifyRevise(positiveConstraint, revisator, reviseCount)) {
            revisator.revised(this, scope(0));
            true
          } else false
        } else true

      case TRUE => noReifyRevise(positiveConstraint, revisator, reviseCount);
      case FALSE => noReifyRevise(negativeConstraint, revisator, reviseCount);

      case _ => throw new IllegalStateException

    }

  private def noReifyRevise(constraint: Constraint,
    revisator: RevisionHandler, reviseCount: Int) = {
    val reifiedRevisator = if (revisator == usualRevisator) {
      usualReifiedRevisator;
    } else {
      new ReifiedRevisionHandler(revisator);
      usualRevisator = revisator;
      usualReifiedRevisator = new ReifiedRevisionHandler(revisator)
      usualReifiedRevisator
    }

    val actualRevise = if (controlRemovals >= reviseCount) {
      -1;
    } else {
      reviseCount;
    }

    if (constraint.revise(reifiedRevisator, actualRevise)) {
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
    positiveConstraint.getEvaluation
  +negativeConstraint.getEvaluation

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

  override def hasNoRemovals(value: Int) =
    controlRemovals < value && positiveConstraint.hasNoRemovals(value);

}
