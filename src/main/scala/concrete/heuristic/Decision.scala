package concrete
package heuristic

sealed trait Decision {
  def apply(ps: ProblemState): (Outcome, Seq[(Variable, Event)])

  def toString(ps: ProblemState): String
}

case class Assign(variable: Variable, value: Int) extends Decision {
  override def toString: String = s"$variable = $value"

  def toString(ps: ProblemState): String = s"${variable.toString(ps)} = $value"

  def apply(ps: ProblemState): (Outcome, Seq[(Variable, Event)]) = (ps.assign(variable, value), Seq((variable, Assignment)))
}

case class Remove(variable: Variable, value: Int) extends Decision {
  override def toString: String = s"$variable /= $value"

  def toString(ps: ProblemState): String = s"${variable.toString(ps)} /= $value"

  def apply(ps: ProblemState): (Outcome, Seq[(Variable, Event)]) = {
    val domain = ps.dom(variable)
    val removed = domain.removeIfPresent(value)
    (ps.updateDom(variable, removed), Seq((variable, InsideRemoval(domain, removed))))
  }
}

case class Reduce(variable: Variable, newDomain: Domain) extends Decision {
  override def toString: String = s"$variable &= $newDomain"

  def toString(ps: ProblemState): String = s"${variable.toString(ps)} &= $newDomain"

  def apply(ps: ProblemState): (Outcome, Seq[(Variable, Event)]) = {
    val domain = ps.dom(variable)
    val intersect = domain & newDomain
    (ps.updateDom(variable, intersect), Seq((variable, InsideRemoval(domain, intersect))))
  }
}

case class DeadEnd(cause: Variable*) extends Decision {
  def toString(ps: ProblemState): String = toString

  override def toString: String = "dead-end" + cause.mkString("(", ", ", ")")

  def apply(ps: ProblemState): (Outcome, Seq[(Variable, Event)]) = (Contradiction(cause), Seq())
}
