package concrete.constraint

final class AdviseCount {
  var count = 0

  def clear(): Unit = {
    count += 1
  }
}

trait AdviseCounts extends Advisable {
  protected var advise: AdviseCount = _

  def register(ac: AdviseCount): this.type = {
    advise = ac
    this
  }

  def adviseCount: Int = {
    assert(advise ne null, s"AdviseCount has not been registered for $this")
    advise.count
  }
}

trait Advisable {
  def register(ac: AdviseCount): this.type
}