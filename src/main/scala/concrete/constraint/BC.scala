package concrete
package constraint


trait BC extends Constraint {

  def advise(ps: ProblemState, event: Event, pos: Int): Int =
    if (event <= BoundRemoval) advise(ps, pos) else -1

  def advise(ps: ProblemState, pos: Int): Int

}
