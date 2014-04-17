package concrete.priorityqueues

class Presence {
  private var current = 0

  def clear(): Unit = { current += 1 }

  def isPresent(e: PTag): Boolean = (e.present == current)

  def setPresent(e: PTag): Unit = (e.present = current)

  def unsetPresent(e: PTag): Unit = (e.present = -1)
}

trait PTag {
  var present = -1
}