package cspfj.constraint.semantic;

import cspfj.constraint.Constraint
import cspfj.constraint.Constraint
import cspfj.BooleanDomain
import cspfj.FALSE
import cspfj.TRUE
import cspfj.UNKNOWNBoolean
import cspfj.Variable
import cspfj.UNSATException
import cspfj.constraint.Removals
import cspfj.EMPTY
import cspfj.AdviseCount

final class ReifiedConstraint(
  controlVariable: Variable,
  val positiveConstraint: Constraint,
  val negativeConstraint: Constraint)
  extends Constraint(controlVariable +: (positiveConstraint.scope ++ negativeConstraint.scope).distinct) {

  require(positiveConstraint.scope forall scope.tail.contains)
  require(negativeConstraint.scope forall scope.tail.contains)

  val positivePositions = {
    val a = Array.fill(arity)(-1)
    for (i <- 0 until positiveConstraint.arity) {
      a(position(positiveConstraint.scope(i))) = i
    }
    a
  }

  val negativePositions = {
    val a = Array.fill(arity)(-1)
    for (i <- 0 until negativeConstraint.arity) {
      a(position(negativeConstraint.scope(i))) = i
    }
    a
  }

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

  def revise() = {
    controlDomain.status match {
      case UNKNOWNBoolean => (
        if (!positiveConstraint.isConsistent) {
          controlDomain.setFalse();
          entail()
          true
        } else false) | (
          if (!negativeConstraint.isConsistent) {
            if (!controlDomain.isUnknown) throw UNSATException.e
            controlDomain.setTrue();
            entail()
            true
          } else false)

      case TRUE => noReifyRevise(positiveConstraint);
      case FALSE => noReifyRevise(negativeConstraint);

      case EMPTY => throw new IllegalStateException

    }
  }

  private def noReifyRevise(constraint: Constraint): Boolean = {
    if (controlRemovals != AdviseCount.count)
      (0 until constraint.arity) foreach constraint.advise

    val c = constraint.revise()
    if (constraint.isEntailed) {
      entail();
    }
    c
  }

  override def checkIndices(t: Array[Int]) = {
    (t(0) == 1) == positiveConstraint.checkIndices(t.tail)
  }

  override def checkValues(t: Array[Int]) = {
    (t(0) == 1) == positiveConstraint.checkValues(t.tail)
  }

  override def toString = scope(0) + " == (" + positiveConstraint + ")";

  var controlRemovals = 0

  var posEval = Integer.MAX_VALUE
  var negEval = Integer.MAX_VALUE

  def advise(position: Int) = {
    if (position == 0) controlRemovals = position
    else {
      posEval = positiveConstraint.advise(positivePositions(position))
      negEval = negativeConstraint.advise(negativePositions(position))
    }
    controlDomain.status match {
      case UNKNOWNBoolean => posEval + negEval
      case TRUE => posEval
      case FALSE => negEval
      case EMPTY => throw new IllegalStateException
    }
  }

  val simpleEvaluation = math.max(positiveConstraint.simpleEvaluation,
    negativeConstraint.simpleEvaluation)
}
