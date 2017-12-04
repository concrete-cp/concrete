package concrete.constraint

import concrete.ProblemState

trait BCCompanion extends Constraint {
  def skipIntervals: Boolean

  def sizeThreshold: Int = 100000
  //
  //  override def revise() = {
  //    if (skipIntervals && intervalsOnly) {
  //      Nil
  //    } else {
  //      super.revise()
  //    }
  //  }

  def skip(ps: ProblemState): Boolean =
    (skipIntervals && intervalsOnly(ps)) || scopeSize(ps) > sizeThreshold

  def skip(ps: ProblemState, scopeSize: Int): Boolean = {
    (skipIntervals && intervalsOnly(ps)) || { scopeSize > sizeThreshold }
  }

}