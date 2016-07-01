package concrete.constraint

import com.typesafe.scalalogging.LazyLogging

import concrete.BooleanDomain
import concrete.Contradiction
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable

final class ReifiedConstraint(
  controlVariable: Variable,
  val positiveConstraint: Constraint,
  val negativeConstraint: Constraint)
    extends Constraint(controlVariable +: (positiveConstraint.scope ++ negativeConstraint.scope).distinct)
    with LazyLogging with StatefulConstraint[java.lang.Boolean] with Advisable {

  require(controlVariable.initDomain.isInstanceOf[BooleanDomain], s"${controlVariable} init domain ${controlVariable.initDomain} is not boolean")

  def init(ps: ProblemState) = {
    ps.dom(controlVariable) match {
      case BooleanDomain.UNKNOWNBoolean => updateState(ps, false)
      case BooleanDomain.TRUE => positiveConstraint.init(ps).andThen(updateState(_, true))
      case BooleanDomain.FALSE => negativeConstraint.init(ps).andThen(updateState(_, true))
      case BooleanDomain.EMPTY => Contradiction //throw new UNSATException(msg = s"${controlVariable.toString(ps)} was empty during ${toString(ps)} init")
    }

  }
  //
  //  /**
  //   *  Only initializes constraint states as some constraints' init may remove values from domains
  //   */
  //  private def initCons(c: Constraint, ps: ProblemState): Outcome = {
  //    /*
  //     *  Keep state but restore domains. Probably won't work if constraint state depends on domain state :(
  //     */
  //    c.init(ps).andThen { consistent =>
  //      c match {
  //        case sc: StatefulConstraint[AnyRef] => ps.updateState(sc, consistent(sc))
  //        case _                              => ps
  //      }
  //
  //    }
  //  }

  override def identify(i: Int): Int = {
    negativeConstraint.identify(positiveConstraint.identify(super.identify(i)))
  }

  private val positiveToReifiedPositions = positiveConstraint.scope.map(position).map {
    case Array(i) => i
  }

  private val reifiedToPositivePositions = {
    val a = Array.fill(arity)(-1)
    for (i <- 0 until positiveConstraint.arity) {
      a(positiveToReifiedPositions(i)) = i
    }
    a
  }

  private val negativeToReifiedPositions = negativeConstraint.scope.map(position).map {
    case Array(i) => i
  }

  private val reifiedToNegativePositions = {
    val a = Array.fill(arity)(-1)
    for (i <- 0 until negativeConstraint.arity) {
      a(negativeToReifiedPositions(i)) = i
    }
    a
  }

  private def init(c: Constraint, ps: ProblemState): Outcome = {
    if (ps(this)) {
      ps
    } else {
      c.init(ps).andThen(updateState(_, true))
    }
  }

  def revise(ps: ProblemState): Outcome = {

    ps.dom(controlVariable) match {
      case BooleanDomain.UNKNOWNBoolean =>
        positiveConstraint.init(ps)
          .andThen { psInitPos =>
            if (positiveConstraint.isConsistent(psInitPos).isState) {
              ps
            } else {
              Contradiction
            }
          }
          .andThen { ps =>
            negativeConstraint.init(ps)
              .andThen { psInitNeg =>
                if (negativeConstraint.isConsistent(psInitNeg).isState) {
                  ps
                } else {
                  Contradiction
                }
              }
              .orElse {
                ps.updateDomNonEmpty(controlVariable, BooleanDomain.TRUE).entail(this)
              }
          }
          .orElse {
            ps.updateDomNonEmpty(controlVariable, BooleanDomain.FALSE).entail(this)
          }

      case BooleanDomain.TRUE =>
        init(positiveConstraint, ps)
          .andThen(positiveConstraint.revise)
          .entailIf(this, mod => mod.isEntailed(positiveConstraint))
      case BooleanDomain.FALSE =>
        init(negativeConstraint, ps)
          .andThen(negativeConstraint.revise)
          .entailIf(this, mod => mod.isEntailed(negativeConstraint))

      case BooleanDomain.EMPTY => throw new IllegalStateException

    }

  }

  override def check(t: Array[Int]) = {
    val rp = (t(0) == 1) == positiveConstraint.check(positiveToReifiedPositions.map(t))

    assert(rp == ((t(0) == 0) == negativeConstraint.check(negativeToReifiedPositions.map(t))))

    rp

  }

  override def toString(ps: ProblemState) =
    s"${controlVariable.toString(ps)} <=> ${positiveConstraint.toString(ps)}${
      Option(data(ps))
        .map {
          if (_) " (initialized)" else ""
        }
        .getOrElse(" (no data)")
    }" // / != (${negativeConstraint.toString(ps)})";

  override def toString = s"${controlVariable} <=> ${positiveConstraint}"

  //var controlRemovals = 0

  override def register(ac: AdviseCount): Unit = {
    Seq(positiveConstraint, negativeConstraint)
      .collect {
        case c: Advisable => c
      }
      .foreach(_.register(ac))
  }

  def advise(ps: ProblemState, position: Int) = {
    //logger.debug(s"Advise ${toString(ps)} : $position")
    if (position == 0) {
      ps.dom(controlVariable) match {
        case BooleanDomain.TRUE => positiveConstraint.adviseAll(ps)
        case BooleanDomain.FALSE => negativeConstraint.adviseAll(ps)
        case BooleanDomain.UNKNOWNBoolean => -1
        case d => throw new IllegalStateException(s"$d is not a valid boolean state")
      }
    } else {
      ps.dom(controlVariable) match {
        case BooleanDomain.UNKNOWNBoolean =>
          val p = positiveConstraint.advise(ps, reifiedToPositivePositions(position))
          val n = negativeConstraint.advise(ps, reifiedToNegativePositions(position))
          if (p < 0) {
            if (n < 0) -1 else n
          } else {
            if (n < 0) p else p + n
          }
        case BooleanDomain.TRUE => positiveConstraint.advise(ps, reifiedToPositivePositions(position))
        case BooleanDomain.FALSE => negativeConstraint.advise(ps, reifiedToNegativePositions(position))
        case BooleanDomain.EMPTY => throw new IllegalStateException
      }
    }
  }

  val simpleEvaluation = positiveConstraint.simpleEvaluation + negativeConstraint.simpleEvaluation
}
