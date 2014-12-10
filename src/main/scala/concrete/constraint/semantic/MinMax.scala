package concrete.constraint.semantic

import concrete.constraint.Constraint
import concrete.Variable

import concrete.Revised
import concrete.Domain
import concrete.util.Interval

abstract class MinMax(result: Variable, vars: Array[Variable], constant: Option[Int]) extends Constraint(result +: vars) {

  def advise(domains: IndexedSeq[Domain], pos: Int): Int = arity

  def in(dom: IndexedSeq[Domain]): Domain = {
    val span = dom.iterator.drop(1).map(_.span).reduce(_ span _)
    val withConstant = constant.map(c => Interval(c, c) span span).getOrElse(span)
    dom(0) & withConstant
  }

  def simpleEvaluation: Int = 2

}

final class Min(result: Variable, vars: Array[Variable], constant: Option[Int])
  extends MinMax(result, vars, constant) {
  type State = Unit
  def initState = Unit
  def this(result: Variable, vars: Array[Variable]) = this(result, vars, None)
  def this(result: Variable, vars: Array[Variable], constant: Int) = this(result, vars, Some(constant))

  def revise(domains: IndexedSeq[Domain], s: State) = {
    val ch = in(domains)

    val minValue = ch.head

    Revised(ch +: domains.tail.map(_.removeUntil(minValue)))
  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == (1 until arity).map(tuple).min
  }

  override def toString(domains: IndexedSeq[Domain], s: State) = domains.mkString(result + " = min(", ", ", ")")
}

final class Max(result: Variable, vars: Array[Variable], constant: Option[Int])
  extends MinMax(result, vars, constant) {
  type State = Unit
  def initState = Unit
  def this(result: Variable, vars: Array[Variable]) = this(result, vars, None)
  def this(result: Variable, vars: Array[Variable], constant: Int) = this(result, vars, Some(constant))

  def revise(domains: IndexedSeq[Domain], s: State) = {
    val ch = in(domains)

    val maxValue = ch.last

    Revised(ch +: domains.tail.map(_.removeAfter(maxValue)))
  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == (1 until arity).map(tuple).max
  }

  override def toString(domains: IndexedSeq[Domain], s: State) = domains.mkString(result + " = min(", ", ", ")")
}