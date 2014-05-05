package concrete.constraint

final class AdviseCount {
  var count = 0
  def clear() {
    count += 1
  }
}

trait AdviseCounts extends Advisable {
  private var advise: AdviseCount = _

  def register(ac: AdviseCount) = {
    advise = ac
  }

  def adviseCount = advise.count
}

trait Advisable {
  def register(ac: AdviseCount): Unit
}