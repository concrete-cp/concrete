package concrete
package constraint

import bitvectors.BitVector

trait Removals extends Constraint with AdviseCounts {

  private var modified = BitVector.empty
  private var timestamp = -1

  final def advise(problemState: ProblemState, event: Event, pos: Int): Int = {
    assert(adviseCount >= 0)

    val eval = getEvaluation(problemState)

    if (eval > 0) {
      //println(s"advising $this")
      if (timestamp != adviseCount) {
        clearMod()
        timestamp = adviseCount
      }

      modified += pos
    }
    eval
  }

  def clearMod(): Unit = modified = BitVector.empty // Arrays.fill(removals, -1)

  final def revise(problemState: ProblemState): Outcome = {
    val r = revise(problemState, modified).dueTo((this, modVars(modified)))
    clearMod()
    r
  }

  def modVars(modified: BitVector): Seq[Variable] = modified.traversable.map(scope).toSeq

  def revise(problemState: ProblemState, modified: BitVector): Outcome

  override def consistent(ps: ProblemState): Outcome = consistent(ps, modified)

  def consistent(ps: ProblemState, modified: BitVector): Outcome = {
    revise(ps, modified) match {
      case _: ProblemState => ps
      case c: Contradiction => c
    }
  }

  def getEvaluation(problemState: ProblemState): Int

  def skip(modified: BitVector): Int = {
    val head = modified.nextSetBit(0)
    if (head < 0) {
      -1
    } else if (modified.nextSetBit(head + 1) < 0) {
      head
    } else {
      -1
    }
  }

  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}
