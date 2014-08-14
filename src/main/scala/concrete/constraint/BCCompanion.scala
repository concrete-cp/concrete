package concrete.constraint

trait BCCompanion extends Removals {
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

  override def advise(pos: Int): Int = {
    if ((skipIntervals && intervalsOnly) || scopeSize > sizeThreshold) {
      -1
    } else {
      super.advise(pos)
    }
  }

}