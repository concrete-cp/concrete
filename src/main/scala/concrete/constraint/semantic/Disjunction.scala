package concrete.constraint.semantic;

import scala.annotation.tailrec
import concrete.BooleanDomain
import concrete.Domain
import concrete.FALSE
import concrete.ReviseOutcome
import concrete.Revised
import concrete.TRUE
import concrete.Variable
import concrete.constraint.Constraint

import concrete.Contradiction

final class Disjunction(scope: Array[Variable],
                        val reverses: IndexedSeq[Boolean]) extends Constraint(scope) {

  type State = Unit
  def initState = Unit

  require(reverses.size == scope.size, "reverses must cover all variables")

  var watch1: Int = _
  var watch2: Int = _

  {
    val init = scope.map(_.initDomain.asInstanceOf[BooleanDomain])
    watch1 = seekWatch(init, -1).get //OrElse(-1)
    watch2 = seekWatch(init, watch1).get //OrElse(-1)
  }

  //if (isTrue(watch1) || isTrue(watch2)) entail()

  def advise(domains: IndexedSeq[Domain], p: Int) = if (p == watch1 || p == watch2) 1 else -1

  def this(scope: Variable*) = this(scope.toArray, new Array[Boolean](scope.size))

  override def check(t: Array[Int]) =
    reverses.zip(t).exists(l => l._1 ^ l._2 == 1)

  override def toString(domains: IndexedSeq[Domain], s: State) =
    "\\/" + (scope, domains, reverses).zipped.map((v, d, r) => (if (r) "-" else "") + v + " " + d).mkString("(", ", ", ")")

  def revise(domains: IndexedSeq[Domain], s: State): ReviseOutcome[Unit] = {
    //println(this.toString + " " + watch1 + " " + watch2)
    //val r: ReviseOutcome[Unit] = 
    val d = domains.asInstanceOf[IndexedSeq[BooleanDomain]]

    //if (watch1 < 0) Contradiction
    //else if (watch2 < 0) setTrue(d, watch1)
    //else 
    if (isTrue(d, watch1) || isTrue(d, watch2)) Revised(domains, true)
    else {
      val r1: ReviseOutcome[Unit] = if (isFalse(d, watch1)) {

        seekWatch(d, watch2) match {
          case Some(w) => {
            watch1 = w
            Revised(d, isTrue(d, w))
          }
          case None => {
            setTrue(d, watch2)
          }
        }

      } else {
        Revised(d, false)
      }

      r1 andThen { (d2, _) =>
        val d = d2.asInstanceOf[IndexedSeq[BooleanDomain]]

        if (isFalse(d, watch2)) {

          seekWatch(d, watch1) match {
            case Some(w) => {
              watch2 = w
              Revised(d, isTrue(d, w))
            }
            case None => {
              setTrue(d, watch1)
            }
          }

        } else {
          Revised(d, false)
        }

      }
    }
  }

  private def isTrue(domains: IndexedSeq[BooleanDomain], position: Int) = {
    if (reverses(position)) domains(position) == FALSE else domains(position) == TRUE
  }

  private def isFalse(domains: IndexedSeq[BooleanDomain], position: Int) =
    if (reverses(position)) domains(position) == TRUE else domains(position) == FALSE

  private def setTrue(domains: IndexedSeq[BooleanDomain], position: Int): ReviseOutcome[Unit] = {
    if (canBeTrue(domains, position)) {

      if (reverses(position)) {
        Revised(domains.updated(position, FALSE), true)
      } else {
        Revised(domains.updated(position, TRUE), true)
      }

    } else {
      Contradiction
    }

  }

  private def canBeTrue(domains: IndexedSeq[BooleanDomain], position: Int) = domains(position).canBe(!reverses(position));

  @tailrec
  private def seekWatch(domains: IndexedSeq[BooleanDomain], excluding: Int, i: Int = arity - 1): Option[Int] = {
    if (i < 0) None
    else if (i != excluding && canBeTrue(domains, i)) Some(i)
    else seekWatch(domains, excluding, i - 1)
  }

  val simpleEvaluation = 3
}
