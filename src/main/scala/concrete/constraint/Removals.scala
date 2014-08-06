package concrete.constraint;

import java.util.Arrays

trait Removals extends Constraint with AdviseCounts {

  val removals = new Array[Int](arity)

  def advise(pos: Int) = {
    removals(pos) = adviseCount
    getEvaluation
  }

  def revise() = {
    val r = revise(modified)
    Arrays.fill(removals, -1)
    r
  }

  def revise(modified: List[Int]): Traversable[Int]

  // scope.iterator.zipWithIndex.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

  def modified: List[Int] = {
    var i = arity - 1
    var mod: List[Int] = Nil
    while (i >= 0) {
      if (removals(i) == adviseCount) {
        mod ::= i
      }
      i -= 1
    }
    mod
  }

  def getEvaluation: Int

  //scope.iterator.zip(removals.iterator).filter(t => t._2 >= reviseCount).map(t => t._1)

}
