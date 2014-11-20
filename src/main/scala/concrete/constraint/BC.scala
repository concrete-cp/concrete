package concrete.constraint

import scala.annotation.tailrec
import concrete.ReviseOutcome
import concrete.Contradiction
import concrete.Revised
import concrete.Domain

trait BC extends Constraint {
  def shave(domains: IndexedSeq[Domain], state: State): ReviseOutcome[State]

  @tailrec
  override final def revise(domains: IndexedSeq[Domain], state: State): ReviseOutcome[State] = {
    shave(domains, state) match {
      case Contradiction => Contradiction
      case ch @ Revised(nd, entailed, ns) =>
        if (hasChanges(domains, nd) && !entailed) revise(nd, ns) else ch
    }

  }
}

trait StatelessBC extends BC {

  type State = Unit

  def initState: State = Unit

  def shave(domains: IndexedSeq[Domain], state: State) = shave(domains)

  def shave(domains: IndexedSeq[Domain]): ReviseOutcome[State]

}
