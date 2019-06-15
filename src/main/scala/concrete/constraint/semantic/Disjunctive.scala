package concrete.constraint.semantic

class Disjunctive {

}

class DisjunctiveTask(var id: Int, var est: Int, var lct: Int, var p: Int) {
  def lst: Int = lct - p

  def ect: Int = est + p

  def getId: Int = id - 1

  def setId(id: Int): Unit = {
    this.id = id
  }

  def hasCompulsoryPart: Boolean = lst < ect
}