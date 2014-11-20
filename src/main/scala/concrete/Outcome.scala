package concrete

sealed trait ReviseOutcome[+A] {
  def andThen[B >: A](f: (IndexedSeq[Domain], B) => ReviseOutcome[B]): ReviseOutcome[B]
}
case class Revised[A](domains: IndexedSeq[Domain], entailed: Boolean = false, state: A = Unit) extends ReviseOutcome[A] {
  def andThen[B >: A](f: (IndexedSeq[Domain], B) => ReviseOutcome[B]) = if (entailed) this else f(domains, state)
}

sealed trait FilterOutcome
case object Contradiction extends FilterOutcome with ReviseOutcome[Nothing] {
  def andThen[B >: Nothing](f: (IndexedSeq[Domain], B) => ReviseOutcome[B]) = this
}
case class Filtered[A](newState: ProblemState) extends FilterOutcome
