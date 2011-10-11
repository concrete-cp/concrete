package cspfj.constraint

trait DynamicConstraint extends Constraint {
  def removeTuples(tuple: Array[Int]): Int
  def removeTuple(tuple: Array[Int]): Boolean
}