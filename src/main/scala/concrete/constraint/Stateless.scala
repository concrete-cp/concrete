package concrete.constraint

import concrete.ReviseOutcome
import concrete.Domain
import concrete.Contradiction
import concrete.Revised

trait Stateless extends Constraint {
  type State = Unit

  def initState: State = Unit

  def revise(domains: IndexedSeq[Domain]): ReviseOutcome[Unit]

  def revise(domains: IndexedSeq[Domain], s: State): ReviseOutcome[State] = revise(domains)

  def isConsistent(domains: IndexedSeq[Domain]): Boolean = revise(domains) match {
    case Contradiction      => false
    case Revised(mod, _, _) => mod.forall(_.nonEmpty)
  }

  def toString(domains: IndexedSeq[Domain]): String = this.getClass.getSimpleName +
    (scope, domains).zipped.map((v, d) => s"$v $d").mkString("(", ", ", ")")

  override def toString(domains: IndexedSeq[Domain], s: State) = toString(domains)

  override final def isConsistent(domains: IndexedSeq[Domain], s: State): Boolean = isConsistent(domains)
}