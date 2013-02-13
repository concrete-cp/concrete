package cspfj.constraint.extension

import cspfj.util.Backtrackable
import cspfj.constraint.Removals
import cspfj.Variable
import cspfj.constraint.Constraint
import cspfj.util.SparseSet

class MDDC(_scope: Array[Variable], private val mdd: MDD)
  extends Constraint(_scope) with Removals with Backtrackable[SparseSet] {

  var gNo = new SparseSet(10)

  /**
   * As seen from class MDDC, the missing signatures are as follows. * For
   * convenience, these are usable as stub implementations.
   */
  // Members declared in cspfj.util.Backtrackable 
  def restore(data: cspfj.util.SparseSet): Unit = ???
  def save: cspfj.util.SparseSet = ???
  // Members declared in cspfj.constraint.Constraint
  def checkValues(tuple: Array[Int]): Boolean = ???
  def simpleEvaluation: Int = ???
  // Members declared in cspfj.constraint.Removals
  def getEvaluation: Int = ???
  def revise(modified: List[Int]): Boolean = ???

}