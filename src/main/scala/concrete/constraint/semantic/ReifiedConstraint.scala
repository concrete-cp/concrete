package concrete.constraint.semantic;

import com.typesafe.scalalogging.LazyLogging
import concrete.EMPTY
import concrete.FALSE
import concrete.Outcome
import concrete.ProblemState
import concrete.TRUE
import concrete.UNKNOWNBoolean
import concrete.Variable
import concrete.constraint.Advisable
import concrete.constraint.AdviseCount
import concrete.constraint.Constraint
import concrete.BooleanDomain

final class ReifiedConstraint(
  controlVariable: Variable,
  val positiveConstraint: Constraint,
  val negativeConstraint: Constraint)
    extends Constraint(controlVariable +: (positiveConstraint.scope ++ negativeConstraint.scope).distinct)
    with Advisable with LazyLogging {

  require(controlVariable.initDomain.isInstanceOf[BooleanDomain], s"${controlVariable} init domain ${controlVariable.initDomain} is not boolean")

  override def identify(i: Int): Int = {
    negativeConstraint.identify(positiveConstraint.identify(super.identify(i)))
  }

  private val positiveToReifiedPositions = positiveConstraint.scope.map(position).map {
    case Seq(i) => i
  }

  private val reifiedToPositivePositions = {
    val a = Array.fill(arity)(-1)
    for (i <- 0 until positiveConstraint.arity) {
      a(positiveToReifiedPositions(i)) = i
    }
    a
  }

  private val negativeToReifiedPositions = negativeConstraint.scope.map(position).map {
    case Seq(i) => i
  }

  private val reifiedToNegativePositions = {
    val a = Array.fill(arity)(-1)
    for (i <- 0 until negativeConstraint.arity) {
      a(negativeToReifiedPositions(i)) = i
    }
    a
  }

  def revise(ps: ProblemState): Outcome = {

    ps.dom(controlVariable) match {
      case UNKNOWNBoolean =>
        if (positiveConstraint.isConsistent(ps)) {
          if (negativeConstraint.isConsistent(ps)) {
            ps
          } else {
            //noReifyRevise(true, pd, pd.updated(0, TRUE), state)
            ps.updateDomNonEmpty(controlVariable, TRUE).entail(this)
          }
        } else {
          ps.updateDomNonEmpty(controlVariable, FALSE).entail(this)
        }

      case TRUE  => positiveConstraint.revise(ps)
      case FALSE => negativeConstraint.revise(ps)

      case EMPTY => throw new IllegalStateException

    }

  }

  override def check(t: Array[Int]) = {
    val rp = (t(0) == 1) == positiveConstraint.check(positiveToReifiedPositions.map(t))

    assert(rp == ((t(0) == 0) == negativeConstraint.check(negativeToReifiedPositions.map(t))))

    rp

  }

  override def toString(ps: ProblemState) =
    s"${controlVariable.toString(ps)} == (${positiveConstraint.toString(ps)})" // / != (${negativeConstraint.toString(ps)}";

  override def toString =
    s"${controlVariable} == (${positiveConstraint})"

  //var controlRemovals = 0

  def register(ac: AdviseCount) {
    Seq(positiveConstraint, negativeConstraint).collect {
      case c: Advisable => c.register(ac)
    }
  }

  def advise(ps: ProblemState, position: Int) = {
    //logger.debug(s"Advise ${toString(ps)} : $position")
    if (position == 0) {
      //   controlRemovals = position

      ps.dom(controlVariable) match {
        case TRUE           => positiveConstraint.adviseAll(ps)
        case FALSE          => negativeConstraint.adviseAll(ps)
        case UNKNOWNBoolean => -1
        case d              => throw new IllegalStateException(s"$d is not a valid boolean state")
      }
    } else {
      ps.dom(controlVariable) match {
        case UNKNOWNBoolean => {
          val p = positiveConstraint.advise(ps, reifiedToPositivePositions(position))
          val n = negativeConstraint.advise(ps, reifiedToNegativePositions(position))
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
        case TRUE  => positiveConstraint.advise(ps, reifiedToPositivePositions(position))
        case FALSE => negativeConstraint.advise(ps, reifiedToNegativePositions(position))
        case EMPTY => throw new IllegalStateException
      }
    }

  }

  val simpleEvaluation = positiveConstraint.simpleEvaluation + negativeConstraint.simpleEvaluation
}
