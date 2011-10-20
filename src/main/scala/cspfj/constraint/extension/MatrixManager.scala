package cspfj.constraint.extension;

trait MatrixManager extends Cloneable {
  def removeTuple(tuple: Array[Int]): Boolean

  def check: Boolean

  def supportCondition(position: Int): Boolean

  def getType: String
}