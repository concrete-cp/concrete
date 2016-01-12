package concrete.util

final class TSCache[A] {
  var timestamp: Int = _

  var cached: A = _

  def apply(ts: Int)(op: => A): A = {
    if (timestamp == ts) {
      cached
    } else {
      timestamp = ts
      val els = op
      cached = els
      els
    }
  }

  def apply[B](ts: Int, op: => B, els: => B): B = {
    if (timestamp == ts) {
      op
    } else {
      timestamp = ts
      els
    }
  }
}

final class Timestamp {
  private var timestamp = 0
  def next(): Int = {
    timestamp += 1 //rand.nextInt()
    timestamp
  }
}
