package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import scala.collection.mutable.ArrayBuffer
import concrete.constraint.BCCompanion

object Element {
  def apply(result: Variable, index: Variable, varsIdx: Seq[(Int, Variable)]) = {
    val scope = ArrayBuffer(result, index)

    val lastIndex = varsIdx.map(_._1).max
    val vars = Array.ofDim[Variable](lastIndex + 1)
    val scopeIndex = Array.fill(lastIndex + 1)(-1)
    for ((i, v) <- varsIdx) {
      vars(i) = v
      scopeIndex(i) = scope.size
      scope += v
    }

    Seq(
      new ElementBC(result, index, vars, scopeIndex, scope.toArray),
      new ElementAC(result, index, vars, scopeIndex, scope.toArray))
  }
}

class ElementBC(
  val result: Variable,
  val index: Variable,
  val vars: Array[Variable],
  val scopeIndex: Array[Int],
  scope: Array[Variable])
  extends Constraint(scope) {
  
  def advise(pos: Int): Int = arity
  
  def checkValues(tuple: Array[Int]): Boolean = {
    tuple(1) < arity - 2 && tuple(0) == tuple(tuple(1) + 2)
  }
  def revise(): Traversable[Int] = {
    var ch = List[Int]()
    /**
     * Revise indices
     */
    if (index.dom.filterValues {
      i =>
        i >= 0 && i < vars.length && (vars(i) ne null) &&
          (result.dom.valueInterval intersects vars(i).dom.valueInterval)
    }) {
      ch ::= 1
    }

    /**
     * Revise result
     */
    val union = index.dom.values.map(i => vars(i).dom.valueInterval).reduce(_ union _)
    if (result.dom.intersectVal(union)) {
      ch ::= 0
    }

    /**
     * Revise vars
     */
    if (index.dom.size == 1) {
      val i = index.dom.firstValue
      if (vars(i).dom.intersectVal(result.dom.valueInterval)) {
        ch ::= scopeIndex(i)
      }
    }
    ch
  }
  def simpleEvaluation: Int = 2
}

class ElementAC(
  val result: Variable,
  val index: Variable,
  val vars: Array[Variable],
  val scopeIndex: Array[Int],
  scope: Array[Variable])
  extends Constraint(scope) with BCCompanion {

  def getEvaluation: Int = scopeSize

  def checkValues(tuple: Array[Int]): Boolean = {
    tuple(1) < arity - 2 && tuple(0) == tuple(tuple(1) + 2)
  }

  def skipIntervals = false

  def revise(modified: List[Int]): Traversable[Int] = {
    var ch = List[Int]()
    /**
     * Revise indices
     */
    if (index.dom.filterValues {
      i =>
        i >= 0 && i < vars.length && (vars(i) ne null) &&
          result.dom.values.exists(vars(i).dom.presentVal)
    }) {
      ch ::= 1
    }

    /**
     * Revise result
     */
    val union = index.dom.values.foldLeft(Set[Int]()) {
      (acc, i) => acc ++ vars(i).dom.values
    }
    if (result.dom.filterValues(union)) {
      ch ::= 0
    }

    /**
     * Revise vars
     */
    if (index.dom.size == 1) {
      val i = index.dom.firstValue
      if (vars(i).dom.filterValues(result.dom.presentVal)) {
        ch ::= scopeIndex(i)
      }
    }
    ch
  }
  def simpleEvaluation: Int = 3
}