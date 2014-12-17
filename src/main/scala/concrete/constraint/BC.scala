package concrete.constraint

import scala.annotation.tailrec

import concrete.Contradiction
import concrete.Outcome
import concrete.ProblemState

trait BC extends Constraint {
  def shave(state: ProblemState): Outcome

  @annotation.tailrec
  override final def revise(state: ProblemState): Outcome = {

    shave(state) match {
      case Contradiction => Contradiction
      case ns: ProblemState =>
        if (ns.isEntailed(this)) {
          ns
        } else {
          revise(ns)
        }
    }

  }
}

//trait StatelessBC extends BC {
//
//  type State = Unit
//
//  def initState: State = Unit
//
//  def shave(domains: IndexedSeq[Domain], state: State) = shave(domains)
//
//  def shave(domains: IndexedSeq[Domain]): ReviseOutcome[State]
//
//  def toString(domains: IndexedSeq[Domain]): String = this.getClass.getSimpleName +
//    (scope, domains).zipped.map((v, d) => s"$v $d").mkString("(", ", ", ")")
//
//  override def toString(domains: IndexedSeq[Domain], s: State) = toString(domains)
//
//  def isConsistent(domains: IndexedSeq[Domain]): Boolean = revise(domains, Unit) match {
//    case Contradiction      => false
//    case Revised(mod, _, _) => mod.forall(_.nonEmpty)
//  }
//
//  override final def isConsistent(domains: IndexedSeq[Domain], s: State): Boolean = isConsistent(domains)
//
//}
