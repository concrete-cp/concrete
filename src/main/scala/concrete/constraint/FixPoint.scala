package concrete.constraint

import concrete.ProblemState
import concrete.Contradiction
import concrete.Outcome

trait FixPoint {
  @annotation.tailrec
  final def fixPoint(ps: ProblemState, shave: ProblemState => Outcome): Outcome = {
    shave(ps) match {
      case c: Contradiction => c
      case ns: ProblemState =>
        if (ns eq ps) {
          ns
        } else {
          fixPoint(ns, shave)
        }
    }
  }

  def fixPointM(ps: ProblemState, shavers: IndexedSeq[ProblemState => Outcome]): Outcome = {
    fixPoint(ps, shavers.indices, (ps, i) => shavers(i)(ps))
  }

  def fixPoint(ps: ProblemState, range: Range, shave: (ProblemState, Int) => Outcome): Outcome = {
    if (range.isEmpty) ps
    else {
      val it = Iterator.continually(range).flatten
      var i = it.next()
      var lastModified = i
      var state = shave(ps, i)

      i = it.next()
      while (i != lastModified && state.isState) {
        val ns = shave(state.toState, i)
        if (ns ne state) {
          lastModified = i
          state = ns
        }
        i = it.next()
      }
      state
    }
  }
}