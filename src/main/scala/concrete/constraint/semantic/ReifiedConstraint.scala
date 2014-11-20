package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.constraint.Constraint
import concrete.BooleanDomain
import concrete.FALSE
import concrete.TRUE
import concrete.UNKNOWNBoolean
import concrete.Variable
import concrete.UNSATException
import concrete.constraint.Removals
import concrete.EMPTY
import concrete.constraint.AdviseCount
import concrete.UNSATObject
import concrete.constraint.Advisable
import concrete.ReviseOutcome
import concrete.Filtered
import concrete.Revised
import concrete.Contradiction
import concrete.Domain

final class ReifiedConstraint(
  controlVariable: Variable,
  val positiveConstraint: Constraint,
  val negativeConstraint: Constraint)
  extends Constraint(controlVariable +: (positiveConstraint.scope ++ negativeConstraint.scope).distinct)
  with Advisable {

  type State = (positiveConstraint.State, negativeConstraint.State)

  def initState = (positiveConstraint.initState, negativeConstraint.initState)

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

  private def controlDomain(domains: IndexedSeq[Domain]) = domains.head

  private def positiveDomains(allDomains: IndexedSeq[Domain]) = positivePositions.map(allDomains)
  private def negativeDomains(allDomains: IndexedSeq[Domain]) = negativePositions.map(allDomains)

  def revise(domains: IndexedSeq[Domain], state: State): ReviseOutcome[State] = {
    val sizes = this.sizes(domains)
    controlDomain(domains) match {
      case UNKNOWNBoolean =>
        val pd = positiveDomains(domains)
        val nd = negativeDomains(domains)
        val positive = positiveConstraint.isConsistent(pd, state._1)
        val negative = negativeConstraint.isConsistent(nd, state._2)

        if (positive) {
          if (negative) {
            Revised(domains, false, state)
          } else {
            noReifyRevise(true, pd, domains.updated(0, TRUE), state)
          }
        } else {
          if (negative) {
            noReifyRevise(false, nd, domains.updated(0, FALSE), state)
          } else {
            Contradiction
          }
        }

      case TRUE  => noReifyRevise(true, positiveDomains(domains), domains, state)
      case FALSE => noReifyRevise(false, negativeDomains(domains), domains, state)

      case EMPTY => throw new IllegalStateException

    }

  }
  private def noReifyRevise(
    positive: Boolean,
    constraintDomains: IndexedSeq[Domain],
    allDomains: IndexedSeq[Domain], state: State): ReviseOutcome[State] = {

    if (positive) {
      positiveConstraint.revise(constraintDomains, state._1) match {
        case Contradiction => Contradiction
        case Revised(mod, entail, ns) => Revised((0 until positiveConstraint.arity).foldLeft(allDomains) {
          case (d, p) => d.updated(positivePositions(p), mod(p))
        }, entail, (ns, state._2))
      }
    } else {
      negativeConstraint.revise(constraintDomains, state._2) match {
        case Contradiction => Contradiction
        case Revised(mod, entail, ns) => Revised((0 until negativeConstraint.arity).foldLeft(allDomains) {
          case (d, p) => d.updated(negativePositions(p), mod(p))
        }, entail, (state._1, ns))
      }
    }

  }

  override def check(t: Array[Int]) = {
    (t(0) == 1) == positiveConstraint.check(positivePositions.map(t))
  }

  override def toString = scope(0) + " == (" + positiveConstraint + ")";

  //var controlRemovals = 0

  def register(ac: AdviseCount) {
    Seq(positiveConstraint, negativeConstraint).collect {
      case c: Advisable => c.register(ac)
    }
  }

  def advise(domains: IndexedSeq[Domain], position: Int) = {
    if (position == 0) {
      //   controlRemovals = position
      controlDomain(domains) match {
        case TRUE           => positiveConstraint.adviseAll(positiveDomains(domains))
        case FALSE          => negativeConstraint.adviseAll(negativeDomains(domains))
        case UNKNOWNBoolean => -1
        case _              => throw new IllegalStateException
      }
    } else {
      controlDomain(domains) match {
        case UNKNOWNBoolean => {
          val p = positiveConstraint.advise(positiveDomains(domains), positivePositions(position))
          val n = negativeConstraint.advise(negativeDomains(domains), negativePositions(position))
          if (p < 0) {
            if (n < 0) {
              -1
            } else {
              n
            }
          } else {
            if (n < 0) {
              p
            } else {
              p + n
            }
          }

        }
        case TRUE  => positiveConstraint.advise(positiveDomains(domains), positivePositions(position))
        case FALSE => negativeConstraint.advise(negativeDomains(domains), negativePositions(position))
        case EMPTY => throw new IllegalStateException
      }
    }

  }

  val simpleEvaluation = positiveConstraint.simpleEvaluation + negativeConstraint.simpleEvaluation
}
