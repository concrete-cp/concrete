package concrete.constraint;

import scala.annotation.tailrec
import concrete.Variable
import com.typesafe.scalalogging.LazyLogging
import concrete.Revised
import concrete.ReviseOutcome
import concrete.Contradiction
import concrete.Domain

/**
 * A constraint that can be revised one variable at a time
 */
trait VariablePerVariable extends Removals with LazyLogging {
  type State = Unit

  def initState = Unit

  def isConsistent(domains: IndexedSeq[Domain]): Boolean = revise(domains) != Contradiction
  override def isConsistent(domains: IndexedSeq[Domain], s: State) = isConsistent(domains)
  //
  //  def isConsistent(): Boolean = {
  //    scope foreach { v => v.dom.setLevel(v.dom.currentLevel + 1) }
  //
  //    val result = revise(modified, Unit)
  //    scope foreach { v => v.dom.restoreLevel(v.dom.currentLevel - 1) }
  //
  //    result match {
  //      case Contradiction => false
  //      case _             => true
  //
  //    }
  //  }
  //
  //  override final def isConsistent(s: State) = isConsistent()

  /**
   * Try to filter values from variable getVariable(position).
   *
   * @param position
   * @return true iff any value has been removed
   */
  def reviseVariable(domains: IndexedSeq[Domain], position: Int, modified: List[Int]): Domain

  def revise(domains: IndexedSeq[Domain]): ReviseOutcome[State] = revise(domains, Unit)

  def revise(domains: IndexedSeq[Domain], modified: List[Int], s: State): ReviseOutcome[State] = {
    assert(modified.nonEmpty)
    val tempDomains = domains.toArray
    val skip = if (modified.tail.isEmpty) modified.head else -1

    var p = arity - 1

    while (p >= 0) {
      if (p != skip) {
        tempDomains(p) = reviseVariable(tempDomains, p, modified)
      }
    }

    Revised(tempDomains.toIndexedSeq, isFree(domains))

  }

}
