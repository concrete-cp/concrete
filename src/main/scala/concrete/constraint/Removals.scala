package concrete.constraint;

import concrete.Outcome
import concrete.ProblemState
import cspom.util.BitVector

trait Removals extends Constraint with AdviseCounts {

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

  def revise(problemState: ProblemState): Outcome = {
    val r = revise(problemState, modified)
    clearMod()
    r
  }

  def revise(problemState: ProblemState, modified: BitVector): Outcome

  override def isConsistent(problemState: ProblemState): Outcome = {
    isConsistent(problemState, modified)
  }

  def clearMod(): Unit = modified = BitVector.empty // Arrays.fill(removals, -1)

  def isConsistent(ps: ProblemState, mod: BitVector): Outcome = {
    revise(ps, mod).andThen(_ => ps)
  }

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

  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}
