package concrete
package constraint

import bitvectors.BitVector

trait BC extends Constraint {
  def shave(state: ProblemState): Outcome = ???

  def revise(state: ProblemState): Outcome = {
    fixPoint(state, shave)

  }

  @annotation.tailrec
  final def fixPoint(ps: ProblemState, shave: ProblemState => Outcome): Outcome = {
    shave(ps) match {
      case c: Contradiction => c
      case ns: ProblemState =>
        if (ns eq ps) {
          ns
          // TODO       } else if (ns.isEntailed(this)) {
          //          ns
        } else {
          fixPoint(ns, shave)
        }
    }
  }

  def fixPointM(ps: ProblemState, shavers: IndexedSeq[ProblemState => Outcome]): Outcome = {
    fixPoint(ps, shavers.indices, (ps, i) => shavers(i)(ps))
  }

  def fixPoint(ps: ProblemState, range: Range, shave: (ProblemState, Int) => Outcome): Outcome = {
    //println("start")
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
      //println("contradiction")
      state
    }
    //    @annotation.tailrec
    //    def loop(state: ProblemState, i: Int, lastModified: Int): Outcome = {
    //      if (i == lastModified) {
    //        state
    //      } else {
    //        shave(state, i) match {
    //          case Contradiction => Contradiction
    //          case ns: ProblemState =>
    //            if (ns ne state) {
    //              loop(ns, it.next, i)
    //            } else {
    //              loop(ns, it.next, lastModified)
    //            }
    //        }
    //      }
    //    }
    //
    //    if (it.hasNext) {
    //      val first = it.next
    //      shave(ps, first).andThen { ps =>
    //        loop(ps, it.next, first)
    //      }
    //    } else {
    //      ps
    //    }

  }

  def advise(ps: ProblemState, event: Event, pos: Int) = if (event <= BoundRemoval) advise(ps, pos) else -1
  def advise(ps: ProblemState, pos: Int): Int
}

trait BCRemovals extends Constraint with AdviseCounts with BC {

  var modified = BitVector.empty
  var timestamp = -1

  def advise(problemState: ProblemState, pos: Int): Int = {

    assert(adviseCount >= 0)
    //println(s"advising $this")
    if (timestamp != adviseCount) {
      clearMod()
      timestamp = adviseCount
    }
    modified += pos
    //removals(pos) = adviseCount
    getEvaluation(problemState)

  }

  override def revise(problemState: ProblemState): Outcome = {
    val r = revise(problemState, modified)
    clearMod()
    r
  }

  def revise(ps: ProblemState, modified: BitVector): Outcome

  override def consistent(ps: ProblemState) = consistent(ps, modified)

  def consistent(ps: ProblemState, modified: BitVector): Outcome = {
    revise(ps) match {
      case _: ProblemState => ps
      case c: Contradiction => c
    }
  }

  def clearMod(): Unit = modified = BitVector.empty // Arrays.fill(removals, -1)

  def getEvaluation(problemState: ProblemState): Int

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
