package concrete
package constraint

import bitvectors.BitVector

trait BC extends Constraint with FixPoint {
  def shave(state: ProblemState): Outcome

  def revise(state: ProblemState, mod: BitVector): Outcome = {
    fixPoint(state, shave)
  }

  final def advise(ps: ProblemState, event: Event, pos: Int): Int =
    if (event <= BoundRemoval) advise(ps, pos) else -1

  def advise(ps: ProblemState, pos: Int): Int
}
