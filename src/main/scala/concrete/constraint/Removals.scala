package concrete
package constraint;

import cspom.util.BitVector

trait Removals extends Constraint with AdviseCounts {

  var modified = BitVector.empty
  var timestamp = -1

  def advise(problemState: ProblemState, event: Event, pos: Int): Int = {
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

  def revise(problemState: ProblemState): Outcome = {
    val r = revise(problemState, modified)
    clearMod()
    r
  }

  def revise(problemState: ProblemState, modified: BitVector): Outcome

  override def consistent(ps: ProblemState) = consistent(ps, modified)

  def consistent(ps: ProblemState, modified: BitVector): Outcome = {
    revise(ps, modified) match {
      case _: ProblemState => ps
      case c: Contradiction => c
    }
  }

  def clearMod(): Unit = modified = BitVector.empty // Arrays.fill(removals, -1)

  def getEvaluation(problemState: ProblemState): Int

  def skip(modified: BitVector) = {
    val head = modified.nextSetBit(0)
    if (head < 0) {
      -1
    } else if (modified.nextSetBit(head + 1) < 0) {
      head
    } else {
      -1
    }
  }

  def modVars(modified: BitVector) = modified.traversable.map(scope).toSeq

  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}
