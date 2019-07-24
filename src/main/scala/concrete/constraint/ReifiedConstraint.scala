package concrete.constraint

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete._

final class ReifiedConstraint(
                               val neg: Boolean,
                               controlVariable: Variable,
                               val constraint: Constraint)
  extends Constraint(constraint.scope :+ controlVariable)
    with LazyLogging with Advisable {

  require(controlVariable.initDomain.isInstanceOf[BooleanDomain], s"$controlVariable init domain ${controlVariable.initDomain} is not boolean")
  //  if (scope.forall(_.initDomain.isInstanceOf[BooleanDomain])) {
  //    throw new AssertionError(this.toString)
  //  }

  val simpleEvaluation: Int = constraint.simpleEvaluation

  def init(ps: ProblemState): Outcome = {

    ps.dom(controlVariable) match {
      case BooleanDomain.UNKNOWNBoolean =>
        constraint
          .init(ps)
          .andThen { initialized =>
            assert(initialized.sameDomains(ps), "ReifiedConstraint cannot update domains during init")
            if (initialized.entailed.entailedReif(constraint)) {
              setControl(initialized, value = true).entail(this)
            } else {
              initialized
            }
          }
          .orElse(setControl(ps, value = false).entail(this))

      case d if isTrue(d) => constraint.init(ps)

      case BooleanDomain.EMPTY => Contradiction(controlVariable) //throw new UNSATException(msg = s"${controlVariable.toString(ps)} was empty during ${toString(ps)} init")
      case _ => ps
    }

  }

  private def setControl(ps: ProblemState, value: Boolean) = {
    ps.updateDom(controlVariable, BooleanDomain(value ^ neg))
  }

  private def isTrue(dom: Domain) = if (neg) {
    dom eq BooleanDomain.FALSE
  } else {
    dom eq BooleanDomain.TRUE
  }


  //  private def init(c: Constraint, ps: ProblemState): Outcome = {
  //    if (ps(this)) {
  //      ps
  //    } else {
  //      c.init(ps).andThen(updateState(_, true))
  //    }
  //  }

  override def identify(i: Int): Int = {
    constraint.identify(super.identify(i))
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    //logger.info(s"$mod ${this.toString(ps)}")
    //logger.debug(s"Revision of ${this.toString(ps)}")
    try {
      ps.dom(controlVariable) match {
        case BooleanDomain.UNKNOWNBoolean =>
          constraint.consistent(ps)
            .andThen { ps =>
              if (ps.entailed.entailedReif(constraint)) {
                setControl(ps, value = true).entail(this)
              } else {
                ps
              }
            }
            .orElse {
              setControl(ps, value = false).entail(this) //If(this, _ => arity >= 4)
            }

        case d if isTrue(d) =>
          constraint.revise(ps)
            .entailIf(this, mod => mod.entailed.entailedReif(constraint))

        case BooleanDomain.EMPTY => throw new IllegalStateException

        case _ => ps

      }
    } catch {
      case e: Exception =>
        throw new IllegalStateException(s"revision of ${toString(ps)} failed", e)
    }

  }

  override def toString(ps: ProblemState) =
    s"${if (neg) "¬" else ""}${controlVariable.toString(ps)} => ${constraint.toString(ps)}" // / != (${negativeConstraint.toString(ps)})";

  override def check(t: Array[Int]): Boolean = {
    t.last == (if (neg) 1 else 0) || constraint.check(t.dropRight(1))
  }

  //var controlRemovals = 0

  override def toString = s"${if (neg) "¬" else ""}$controlVariable => $constraint"

  override def register(ac: AdviseCount): this.type = {
    constraint.register(ac)
    super.register(ac)
  }

  override def advise(ps: ProblemState, event: Event, position: Int): Int = 0

  override def event(ps: ProblemState, event: Event, position: Int, alwaysMod: Boolean = false): Int = {
    super.event(ps, event, position) + {
      //logger.debug(s"Advise ${toString(ps)} : $position")
      if (position == constraint.arity) {
        if (isTrue(ps.dom(controlVariable))) {
          constraint.eventAll(ps, alwaysMod = alwaysMod)
        } else {
          -1
        }
      } else {
        val d = ps.dom(controlVariable)
        if ((d eq BooleanDomain.UNKNOWNBoolean) || isTrue(d)) {
          constraint.event(ps, event, position, alwaysMod)
        } else {
          -1
        }
      }
    }
    // logger.info(s"event $position $r ${this.toString(ps)}")

  }
}
