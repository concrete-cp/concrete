package concrete

import concrete.constraint.Constraint

final class AdviseCount {
  var count = 0
  def clear() {
    count += 1
  }
}

trait AdviseCounts {
  private var advise: AdviseCount = _
  
  def register(ac: AdviseCount) = {
    advise = ac
  }
  
  def adviseCount = advise.count
}
