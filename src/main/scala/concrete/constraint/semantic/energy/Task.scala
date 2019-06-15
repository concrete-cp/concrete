package concrete.constraint.semantic.energy


class Task(var id: Int, var est: Int, var lct: Int, var p: Int, var h: Int = 1) {

  def this(task: Task) {
    this(task.getId + 1, task.est, task.lct, task.p, task.h)
  }

  def getId: Int = id - 1

  def setId(id: Int): Unit = {
    this.id = id
  }

  def hasCompulsoryPart: Boolean = lst < ect

  def getEnv(C: Int): Int = C * est + e

  def getEnvc(C: Int, c: Int): Int = (C - c) * est + e

  def e: Int = p * h

  def computeMinimumIntersection(t1: Int, t2: Int): Int =
    Math.min(computeLeftShift(t1, t2), computeRightShift(t1, t2))

  def computeLeftShift(t1: Int, t2: Int): Int =
    Math.max(0, h * (Math.min(t2, ect) - Math.max(t1, est)))

  def computeRightShift(t1: Int, t2: Int): Int =
    Math.max(0, h * (Math.min(t2, lct) - Math.max(t1, lst)))

  override def toString: String =
    s"Id: $id, est: $est, ect: $ect, lst: $lst, lct: $lct, p: $p, h: $h, est+lct: ${ect + lct}"

  def lst: Int = lct - p

  def ect: Int = est + p
}
