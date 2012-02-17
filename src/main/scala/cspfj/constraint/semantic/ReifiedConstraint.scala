package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.constraint.Constraint
import cspfj.problem.BooleanDomain
import cspfj.problem.FALSE
import cspfj.problem.TRUE
import cspfj.problem.UNKNOWN
import cspfj.problem.Variable
import cspfj.UNSATException
import cspfj.constraint.Removals

final class ReifiedConstraint(
  controlVariable: Variable,
  val positiveConstraint: Constraint,
  val negativeConstraint: Constraint)
  extends AbstractConstraint(controlVariable +: (positiveConstraint.scope ++ negativeConstraint.scope).distinct) {

  require(positiveConstraint.scope forall scope.tail.contains)
  require(negativeConstraint.scope forall scope.tail.contains)

  val positivePositions = (0 until positiveConstraint.arity) map { i =>
    position(positiveConstraint.scope(i)) -> i
  } toMap

  val negativePositions = (0 until negativeConstraint.arity) map { i =>
    position(negativeConstraint.scope(i)) -> i
  } toMap

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

  def revise() {
    controlDomain.status match {
      case UNKNOWN =>
        if (!positiveConstraint.isConsistent) {
          controlDomain.setFalse();
          entail()
        }
        if (!negativeConstraint.isConsistent) {
          if (!controlDomain.isUnknown) throw UNSATException.e
          controlDomain.setTrue();
          entail()
        }

      case TRUE => noReifyRevise(positiveConstraint);
      case FALSE => noReifyRevise(negativeConstraint);

      case _ => throw new IllegalStateException

    }
  }

  private def noReifyRevise(constraint: Constraint) {
    if (controlRemovals != Removals.count)
      constraint.fillRemovals()

    constraint.revise()
    if (constraint.isEntailed) {
      entail();
    }

  }

  def check = {
    tuple.copyToArray(positiveConstraint.tuple, 1, arity - 1)
    (value(0) == 1) == positiveConstraint.check
  }

  def getEvaluation = controlDomain.status match {
    case UNKNOWN => positiveConstraint.getEvaluation + negativeConstraint.getEvaluation
    case TRUE => positiveConstraint.getEvaluation
    case FALSE => negativeConstraint.getEvaluation
    case _ => throw new IllegalStateException
  }

  override def toString = scope(0) + " == (" + positiveConstraint + ")";

  var controlRemovals = 0

  override def setRemovals(position: Int) {
    if (position == 0) controlRemovals = position
    else {
      positivePositions.get(position) match {
        case Some(p) => positiveConstraint.setRemovals(p)
        case None =>
      }
      negativePositions.get(position) match {
        case Some(p) => negativeConstraint.setRemovals(p)
        case None =>
      }
    }
  }

  override def clearRemovals() {
    positiveConstraint.clearRemovals();
    negativeConstraint.clearRemovals();
  }

  override def fillRemovals() {
    positiveConstraint.fillRemovals()
    negativeConstraint.fillRemovals()
  }
}
