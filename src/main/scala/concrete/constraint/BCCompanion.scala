package concrete.constraint

import concrete.Domain
import concrete.ProblemState

trait BCCompanion extends Constraint {
  def skipIntervals: Boolean

  def sizeThreshold: Int = 1000
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

}