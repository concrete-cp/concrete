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
import com.typesafe.scalalogging.LazyLogging

final class ReifiedConstraint(
  controlVariable: Variable,
  val positiveConstraint: Constraint,
  val negativeConstraint: Constraint)
  extends Constraint(controlVariable +: (positiveConstraint.scope ++ negativeConstraint.scope).distinct)
  with Advisable with LazyLogging {

  type State = (positiveConstraint.State, negativeConstraint.State)

  def initState = (positiveConstraint.initState, negativeConstraint.initState)

  private val positiveToReifiedPositions = positiveConstraint.scope.map(position)

  private val reifiedToPositivePositions = {
    val a = Array.fill(arity)(-1)
    for (i <- 0 until positiveConstraint.arity) {
      a(positiveToReifiedPositions(i)) = i
    }
    a
  }

  private val negativeToReifiedPositions = negativeConstraint.scope.map(position)

  private val reifiedToNegativePositions = {
    val a = Array.fill(arity)(-1)
    for (i <- 0 until negativeConstraint.arity) {
      a(negativeToReifiedPositions(i)) = i
    }
    a
  }

  private def controlDomain(domains: IndexedSeq[Domain]) = domains.head

  private def positiveDomains(allDomains: IndexedSeq[Domain]) = positiveToReifiedPositions.map(allDomains)
  private def negativeDomains(allDomains: IndexedSeq[Domain]) = negativeToReifiedPositions.map(allDomains)

  def revise(domains: IndexedSeq[Domain], state: State): ReviseOutcome[State] = {

    controlDomain(domains) match {
      case UNKNOWNBoolean =>

        if (positiveConstraint.isConsistent(positiveDomains(domains), state._1)) {
          if (negativeConstraint.isConsistent(negativeDomains(domains), state._2)) {
            Revised(domains, false, state)
          } else {
            //noReifyRevise(true, pd, pd.updated(0, TRUE), state)
            Revised(domains.updated(0, TRUE), true, state)
          }
        } else {
          negativeRevise(domains.updated(0, FALSE), state) match {
            case Contradiction => Contradiction
            case Revised(mod, e, s) =>
              if (!e) logger.warn(toString(domains, state) + " -> " + toString(mod, s) + " should be entailed")
              Revised(mod, true, s)
          }

        }

      case TRUE  => positiveRevise(domains, state)
      case FALSE => negativeRevise(domains, state)

      case EMPTY => throw new IllegalStateException

    }

  }

  private def positiveRevise(domains: IndexedSeq[Domain], state: State): ReviseOutcome[State] = {
    val constraintDomains = positiveDomains(domains)
    positiveConstraint.revise(constraintDomains, state._1) match {
      case Contradiction => Contradiction
      case Revised(mod, entail, ns) => Revised((0 until positiveConstraint.arity).foldLeft(domains) {
        case (d, p) => d.updated(positiveToReifiedPositions(p), mod(p))
      }, entail, (ns, state._2))
    }
  }

  private def negativeRevise(domains: IndexedSeq[Domain], state: State): ReviseOutcome[State] = {
    val constraintDomains = negativeDomains(domains)
    negativeConstraint.revise(constraintDomains, state._2) match {
      case Contradiction => Contradiction
      case Revised(mod, entail, ns) => Revised((0 until negativeConstraint.arity).foldLeft(domains) {
        case (d, p) => d.updated(negativeToReifiedPositions(p), mod(p))
      }, entail, (state._1, ns))
    }
  }

  override def check(t: Array[Int]) = {
    (t(0) == 1) == positiveConstraint.check(positiveToReifiedPositions.map(t))
  }

  override def toString(domains: IndexedSeq[Domain], state: State) =
    s"$controlVariable ${domains(0)} == (${positiveConstraint.toString(positiveDomains(domains), state._1)}) / != (${negativeConstraint.toString(negativeDomains(domains), state._2)}";

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
          val p = positiveConstraint.advise(positiveDomains(domains), reifiedToPositivePositions(position))
          val n = negativeConstraint.advise(negativeDomains(domains), reifiedToNegativePositions(position))
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
        case TRUE  => positiveConstraint.advise(positiveDomains(domains), reifiedToPositivePositions(position))
        case FALSE => negativeConstraint.advise(negativeDomains(domains), reifiedToNegativePositions(position))
        case EMPTY => throw new IllegalStateException
      }
    }

  }

  val simpleEvaluation = positiveConstraint.simpleEvaluation + negativeConstraint.simpleEvaluation
}
