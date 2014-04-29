package concrete.util

trait Backtrackable[A] extends DelayedInit {

  private var history: List[(Int, A)] = Nil

  def delayedInit(x: => Unit) {
    x
    history = List((0, save))
  }

  private var _currentLevel = 0

  private var _altered = true

  def currentLevel = _currentLevel

  def altering() {
    _altered = true
  }

  def getHistory = history

  def save: A

  def restore(data: A)

  def setLevel(level: Int) {
    assert(level > currentLevel, "Given level " + level
      + " should be greater than current " + currentLevel)
    if (_altered) {
      history ::= ((currentLevel, save))
      _altered = false
    }
    _currentLevel = level;
  }

  def restoreLevel(level: Int) {
    assert(level <= currentLevel);

    //if (history != Nil) {
    history = history.dropWhile(_._1 > level)
    //    if (history == Nil) {
    //      restore(init())
    //    } else {
    assert(!history.isEmpty)
    restore(history.head._2)
    //}
    _altered = false
    //    }
    _currentLevel = level;
    //    _last = bvDomain.prevSetBit(domain.length);

  }

  def getLevel(level: Int) = {
    if (level < currentLevel) {
      history.find(_._1 <= level).get._2
    } else { save }
  }

}
