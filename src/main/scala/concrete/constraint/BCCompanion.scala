package concrete.constraint

import concrete.Domain

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

  def skip(domains: IndexedSeq[Domain]): Boolean =
    (skipIntervals && intervalsOnly(domains)) || scopeSize(domains) > sizeThreshold

}