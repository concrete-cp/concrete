package concrete.priorityqueues

object PTag {
  var current = 0

  def clear() { current += 1 }
}

trait PTag {
  private var present = -1

  def isPresent = present == PTag.current

  def setPresent() { present = PTag.current }

  def unsetPresent() { present = -1 }
}