package concrete.constraint

import com.typesafe.scalalogging.LazyLogging

import concrete.BooleanDomain
import concrete.Contradiction
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.Event

final class ReifiedConstraint(
  controlVariable: Variable,
  val positiveConstraint: Constraint,
  val negativeConstraint: Constraint)
    extends Constraint(controlVariable +: (positiveConstraint.scope ++ negativeConstraint.scope).distinct)
    with LazyLogging with Advisable {

  require(controlVariable.initDomain.isInstanceOf[BooleanDomain], s"${controlVariable} init domain ${controlVariable.initDomain} is not boolean")

//  if (scope.forall(_.initDomain.isInstanceOf[BooleanDomain])) {
//    throw new AssertionError(this.toString)
//  }

  def init(ps: ProblemState) = {

    ps.dom(controlVariable) match {
      case BooleanDomain.UNKNOWNBoolean =>
        val initialized = positiveConstraint
          .init(ps)
          .orElse(ps.updateDomNonEmpty(controlVariable, BooleanDomain.FALSE))
          .andThen { initPos =>
            negativeConstraint
              .init(initPos)
              .orElse(initPos.updateDomNonEmpty(controlVariable, BooleanDomain.TRUE))
          }

        require(initialized.domainsOption == ps.domainsOption, "ReifiedConstraint cannot update domains during init")

        initialized

      case BooleanDomain.TRUE => positiveConstraint.init(ps)
      case BooleanDomain.FALSE => negativeConstraint.init(ps)
      case BooleanDomain.EMPTY => Contradiction(scope, Seq(controlVariable)) //throw new UNSATException(msg = s"${controlVariable.toString(ps)} was empty during ${toString(ps)} init")
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

  //  private def init(c: Constraint, ps: ProblemState): Outcome = {
  //    if (ps(this)) {
  //      ps
  //    } else {
  //      c.init(ps).andThen(updateState(_, true))
  //    }
  //  }

  def revise(ps: ProblemState): Outcome = {

    ps.dom(controlVariable) match {
      case BooleanDomain.UNKNOWNBoolean =>
        positiveConstraint.consistent(ps)
          .andThen { ps =>
            negativeConstraint.consistent(ps)
              .orElse {
                ps.updateDomNonEmpty(controlVariable, BooleanDomain.TRUE).entail(this) //entailIf(this, _ => arity >= 4)
              }
          }
          .orElse {
            ps.updateDomNonEmpty(controlVariable, BooleanDomain.FALSE).entail(this) //If(this, _ => arity >= 4)
          }

      case BooleanDomain.TRUE =>
        positiveConstraint.revise(ps)
      //.entailIf(this, mod => mod.isEntailed(positiveConstraint))
      case BooleanDomain.FALSE =>
        negativeConstraint.revise(ps)
      //.entailIf(this, mod => mod.isEntailed(negativeConstraint))

      case BooleanDomain.EMPTY => throw new IllegalStateException

    }

  }

  override def check(t: Array[Int]) = {
    val rp = (t(0) == 1) == positiveConstraint.check(positiveToReifiedPositions.map(t))

    assert(rp == ((t(0) == 0) == negativeConstraint.check(negativeToReifiedPositions.map(t))))

    rp

  }

  override def toString(ps: ProblemState) =
    s"${controlVariable.toString(ps)} <=> ${positiveConstraint.toString(ps)}" // / != (${negativeConstraint.toString(ps)})";

  override def toString = s"${controlVariable} <=> ${positiveConstraint}"

  //var controlRemovals = 0

  override def register(ac: AdviseCount): Unit = {
    Seq(positiveConstraint, negativeConstraint)
      .collect {
        case c: Advisable => c
      }
      .foreach(_.register(ac))
  }

  def advise(ps: ProblemState, event: Event, position: Int) = {
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

          val p = positiveConstraint.advise(ps, event, reifiedToPositivePositions(position))
          val n = negativeConstraint.advise(ps, event, reifiedToNegativePositions(position))
          if (p < 0) {
            if (n < 0) -1 else n
          } else {
            if (n < 0) p else p + n
          }
        case BooleanDomain.TRUE => positiveConstraint.advise(ps, event, reifiedToPositivePositions(position))
        case BooleanDomain.FALSE => negativeConstraint.advise(ps, event, reifiedToNegativePositions(position))
        case BooleanDomain.EMPTY => throw new IllegalStateException
      }
    }
  }

  val simpleEvaluation = positiveConstraint.simpleEvaluation + negativeConstraint.simpleEvaluation
}
