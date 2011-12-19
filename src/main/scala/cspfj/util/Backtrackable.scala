package cspfj.util

trait Backtrackable[A] {

  private var history: List[(Int, A)] = List((-1, save))

  private var currentLevel = 0

  private var _altered = false

  def altered() {
    _altered = true
  }

  def save: A

  def restore(data: A)

  def setLevel(level: Int) {
    assert(level > currentLevel, "Given level " + level
      + " should be greater than current " + currentLevel)
    if (_altered) {
      history ::= (currentLevel, save)
      _altered = false
    }
    currentLevel = level;
  }

  def restoreLevel(level: Int) {
    assert(level <= currentLevel);

    history = history.dropWhile(_._1 > level)
    //    if (history == Nil) {
    //      restore(init())
    //    } else {
    assert(!history.isEmpty)
    restore(history.head._2)
    //    }
    currentLevel = level;
    //    _last = bvDomain.prevSetBit(domain.length);

  }

  def getLevel(level: Int): Option[A] = {
    if (level < currentLevel) {
      history.find(_._1 <= level) match {
        case Some(e) => Some(e._2)
        case _ => throw new IllegalStateException
      }
    } else None
  }

}