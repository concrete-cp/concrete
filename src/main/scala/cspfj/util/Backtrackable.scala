package cspfj.util

trait Backtrackable[A] {

  private var history: List[(Int, A)] = Nil //List((-1, save))

  private var currentLevel = 0

  //private var _altered = false

  private var saved = false

  def altering() {
    if (!saved) {
      history ::= (currentLevel - 1, save)
      saved = true
    }
  }

  def save: A

  def restore(data: A)

  def setLevel(level: Int) {
    assert(level > currentLevel, "Given level " + level
      + " should be greater than current " + currentLevel)
    saved = false
    currentLevel = level;
  }

  def restoreLevel(level: Int) {
    assert(level <= currentLevel);

    if (history != Nil) {

      history = history.dropWhile(_._1 > level)
      //    if (history == Nil) {
      //      restore(init())
      //    } else {
      // assert(!history.isEmpty)
      //if (history != Nil)
      assert(!history.isEmpty, "History of " + this + " emptied !")
      restore(history.head._2)
    }
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