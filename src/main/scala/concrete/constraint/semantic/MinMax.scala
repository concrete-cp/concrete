package concrete.constraint.semantic

import concrete.constraint.Constraint
import concrete.Variable
import concrete.constraint.Stateless
import concrete.Revised
import concrete.Domain
import concrete.util.Interval

abstract class MinMax(result: Variable, vars: Array[Variable], constant: Option[Int]) extends Constraint(result +: vars) with Stateless {

  def advise(domains: IndexedSeq[Domain], pos: Int): Int = arity

  def in(dom: IndexedSeq[Domain]): IndexedSeq[Domain] = {
    val span = dom.iterator.drop(1).map(_.span).reduce(_ span _)
    val withConstant = constant.map(c => Interval(c, c) span span).getOrElse(span)
    val result = dom(0) & withConstant
    if (result ne dom(0)) dom.updated(0, result) else dom
  }

  def simpleEvaluation: Int = 2

}

final class Min(result: Variable, vars: Array[Variable], constant: Option[Int])
  extends MinMax(result, vars, constant) {

  def this(result: Variable, vars: Array[Variable]) = this(result, vars, None)
  def this(result: Variable, vars: Array[Variable], constant: Int) = this(result, vars, Some(constant))

  def revise(domains: IndexedSeq[Domain]) = {
    var ch = in(domains)

    val minValue = ch(0).head

    Revised(ch.head +: ch.tail.map(_.removeUntil(minValue)))
  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == (1 until arity).map(tuple).min
  }

  override def toString = vars.mkString(result + " = min(", ", ", ")")
}

final class Max(result: Variable, vars: Array[Variable], constant: Option[Int])
  extends MinMax(result, vars, constant) {

  def this(result: Variable, vars: Array[Variable]) = this(result, vars, None)
  def this(result: Variable, vars: Array[Variable], constant: Int) = this(result, vars, Some(constant))

  def revise(domains: IndexedSeq[Domain]) = {
    var ch = in(domains)

    val maxValue = ch(0).last

    Revised(ch.head +: ch.tail.map(_.removeAfter(maxValue)))
  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) == (1 until arity).map(tuple).max
  }

  override def toString = vars.mkString(result + " = min(", ", ", ")")
}