package concrete.constraint.semantic

import concrete.constraint.Constraint
import concrete.Variable

abstract class MinMax(result: Variable, vars: Array[Variable], constant: Option[Int]) extends Constraint(result +: vars) {

  def advise(pos: Int): Int = arity

  def maxOfVars = {
    var m = constant
    for (v <- vars) {
      val value = v.dom.lastValue
      if (m.forall(_ < value)) {
        m = Some(value)
      }
    }
    m.get
  }

  def minOfVars = {
    var m = constant
    for (v <- vars) {
      val value = v.dom.firstValue
      if (m.forall(_ > value)) {
        m = Some(value)
      }
    }
    m.get
  }

  def in(): List[Int] = {
    if (result.dom.removeAfterVal(maxOfVars) | result.dom.removeUntilVal(minOfVars)) {
      List(0)
    } else {
      Nil
    }
  }

  def simpleEvaluation: Int = 2

}

final class Min(result: Variable, vars: Array[Variable], constant: Option[Int])
  extends MinMax(result, vars, constant) {

  def this(result: Variable, vars: Array[Variable]) = this(result, vars, None)
  def this(result: Variable, vars: Array[Variable], constant: Int) = this(result, vars, Some(constant))

  def revise(): Traversable[Int] = {
    var ch = in()

    val minValue = result.dom.firstValue

    for (i <- 1 until arity) {
      if (scope(i).dom.removeUntilVal(minValue)) {
        ch ::= i
      }
    }

    ch
  }

  def checkValues(tuple: Array[Int]): Boolean = {
    tuple(0) == (1 until arity).map(tuple).min
  }

  override def toString = vars.mkString(result + " = min(", ", ", ")")
}

final class Max(result: Variable, vars: Array[Variable], constant: Option[Int])
  extends MinMax(result, vars, constant) {

  def this(result: Variable, vars: Array[Variable]) = this(result, vars, None)
  def this(result: Variable, vars: Array[Variable], constant: Int) = this(result, vars, Some(constant))

  def revise(): Traversable[Int] = {
    var ch = in()

    val maxValue = result.dom.lastValue

    for (i <- 1 until arity) {
      if (scope(i).dom.removeAfterVal(maxValue)) {
        ch ::= i
      }
    }

    ch
  }

  def checkValues(tuple: Array[Int]): Boolean = {
    tuple(0) == (1 until arity).map(tuple).max
  }

  override def toString = vars.mkString(result + " = min(", ", ", ")")
}