package concrete.constraint

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete._

final class ReifiedConstraint(
                               controlVariable: Variable,
                               val positiveConstraint: Constraint,
                               val negativeConstraint: Constraint)
  extends Constraint(controlVariable +: (positiveConstraint.scope ++ negativeConstraint.scope).distinct)
    with LazyLogging with Advisable {

  require(controlVariable.initDomain.isInstanceOf[BooleanDomain], s"$controlVariable init domain ${controlVariable.initDomain} is not boolean")
  //  if (scope.forall(_.initDomain.isInstanceOf[BooleanDomain])) {
  //    throw new AssertionError(this.toString)
  //  }

  val simpleEvaluation: Int = positiveConstraint.simpleEvaluation + negativeConstraint.simpleEvaluation

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

  def init(ps: ProblemState): Outcome = {

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

        assert(initialized.toState.domains == ps.domains, "ReifiedConstraint cannot update domains during init")

        initialized

      case BooleanDomain.TRUE => positiveConstraint.init(ps)
      case BooleanDomain.FALSE => negativeConstraint.init(ps)
      case BooleanDomain.EMPTY => Contradiction(controlVariable) //throw new UNSATException(msg = s"${controlVariable.toString(ps)} was empty during ${toString(ps)} init")
    }

  }

  //  private def init(c: Constraint, ps: ProblemState): Outcome = {
  //    if (ps(this)) {
  //      ps
  //    } else {
  //      c.init(ps).andThen(updateState(_, true))
  //    }
  //  }

  override def identify(i: Int): Int = {
    negativeConstraint.identify(positiveConstraint.identify(super.identify(i)))
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    //logger.info(s"$mod ${this.toString(ps)}")
    //logger.debug(s"Revision of ${this.toString(ps)}")
    try {
      ps.dom(controlVariable) match {
        case BooleanDomain.UNKNOWNBoolean =>
          positiveConstraint.consistent(ps)
            .andThen { ps =>
              if (ps.entailed.entailedReif(positiveConstraint)) {
                ps.updateDomNonEmpty(controlVariable, BooleanDomain.TRUE).entail(this)
              } else {
                negativeConstraint.consistent(ps)
                  .orElse {
                    ps.updateDomNonEmpty(controlVariable, BooleanDomain.TRUE).entail(this) //entailIf(this, _ => arity >= 4)
                  }
              }
            }
            .orElse {
              ps.updateDomNonEmpty(controlVariable, BooleanDomain.FALSE).entail(this) //If(this, _ => arity >= 4)
            }

        case BooleanDomain.TRUE =>
          positiveConstraint.revise(ps)
            .entailIf(this, mod => mod.entailed.entailedReif(positiveConstraint))
        case BooleanDomain.FALSE =>

          negativeConstraint.revise(ps)
            .entailIf(this, mod => mod.entailed.entailedReif(negativeConstraint))


        case BooleanDomain.EMPTY => throw new IllegalStateException

      }
    } catch {
      case e: Exception =>
        throw new IllegalStateException(s"revision of ${toString(ps)} failed", e)
    }

  }

  override def check(t: Array[Int]): Boolean = {
    val rp = (t(0) == 1) == positiveConstraint.check(positiveToReifiedPositions.map(t))

    assert(rp == ((t(0) == 0) == negativeConstraint.check(negativeToReifiedPositions.map(t))))

    rp

  }

  override def toString = s"$controlVariable <=> $positiveConstraint"

  //var controlRemovals = 0

  override def register(ac: AdviseCount): this.type = {
    positiveConstraint.register(ac)
    negativeConstraint.register(ac)
    super.register(ac)
    //    Seq(positiveConstraint, negativeConstraint)
    //      .collect {
    //        case c: Advisable => c
    //      }
    //      .foreach(_.register(ac))
  }

  override def advise(ps: ProblemState, event: Event, position: Int): Int = 0

  override def event(ps: ProblemState, event: Event, position: Int, alwaysMod: Boolean = false): Int = {
    val r = super.event(ps, event, position) + {
      //logger.debug(s"Advise ${toString(ps)} : $position")
      if (position == 0) {
        ps.dom(controlVariable) match {
          case BooleanDomain.TRUE => positiveConstraint.eventAll(ps, alwaysMod = alwaysMod)
          case BooleanDomain.FALSE => negativeConstraint.eventAll(ps, alwaysMod = alwaysMod)
          case BooleanDomain.UNKNOWNBoolean => -1
          case d => throw new IllegalStateException(s"$d is not a valid boolean state")
        }
      } else {

        ps.dom(controlVariable) match {
          case BooleanDomain.UNKNOWNBoolean =>

            val p = positiveConstraint.event(ps, event, reifiedToPositivePositions(position), alwaysMod)
            val n = negativeConstraint.event(ps, event, reifiedToNegativePositions(position), alwaysMod)
            if (p < 0) {
              n
            } else if (n < 0) {
              p
            } else {
              p + n
            }
          case BooleanDomain.TRUE => positiveConstraint.event(ps, event, reifiedToPositivePositions(position), alwaysMod)
          case BooleanDomain.FALSE => negativeConstraint.event(ps, event, reifiedToNegativePositions(position), alwaysMod)
          case BooleanDomain.EMPTY => throw new IllegalStateException
        }
      }
    }
    // logger.info(s"event $position $r ${this.toString(ps)}")
    r
  }

  override def toString(ps: ProblemState) =
    s"${controlVariable.toString(ps)} <=> ${positiveConstraint.toString(ps)}" // / != (${negativeConstraint.toString(ps)})";
}
