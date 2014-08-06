package concrete.constraint


trait BCCompanion extends Removals {
  def skipIntervals: Boolean
//
//  override def revise() = {
//    if (skipIntervals && intervalsOnly) {
//      Nil
//    } else {
//      super.revise()
//    }
//  }

  override def advise(pos: Int): Int = {
    if (skipIntervals && intervalsOnly) {
      -1
    } else {
      super.advise(pos)
    }
  }

}