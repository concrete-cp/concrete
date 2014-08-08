package concrete.constraint.semantic

import scala.annotation.tailrec
import concrete.constraint.Constraint
import concrete.Domain
import concrete.util.Interval
import concrete.Variable
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable.HashSet
import concrete.constraint.BC

import SumMode._

final class Sum(
  val constant: Int,
  val factors: Array[Int],
  scope: Array[Variable],
  mode: SumMode) extends Constraint(scope)
  with LazyLogging {

  def this(constant: Int, scope: Array[Variable], mode: SumMode) =
    this(constant, Array.fill(scope.length)(1), scope, mode)

  def checkValues(t: Array[Int]): Boolean = {
    val sum = (0 until arity).map(i => t(i) * factors(i)).sum
    mode match {
      case SumLE => sum <= constant
      case SumLT => sum < constant
      case SumEQ => sum == constant
      // case SumNE => sum != constant
    }
  }

  def advise(p: Int) = arity

  val initBound = Interval(-constant, -constant)

  private def filter(dom: Domain, itv: Interval, neg: Boolean): Boolean = mode match {
    case SumLE if neg => dom.removeUntilVal(itv.lb)
    case SumLE => dom.removeAfterVal(itv.ub)
    case SumEQ => dom.intersectVal(itv)
  }

  def revise(): Traversable[Int] = {

    var ch = new HashSet[Int]()

    var bounds = initBound

    var i = arity - 1

    while (i >= 0) {
      bounds += scope(i).dom.valueInterval * factors(i)
      i -= 1
    }

    var change = true
    while (change) {
      change = false

      i = arity - 1
      while (i >= 0) {
        val dom = scope(i).dom
        val f = factors(i)
        val myBounds = dom.valueInterval * f

        bounds = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub)

        if (filter(dom, bounds / -f, f < 0)) {
          ch += i
          change = true
        }

        bounds += dom.valueInterval * f

        i -= 1
      }
    }
    ch
  }

  override def toString = (scope, factors).zipped.map((v, f) => f + "." + v).mkString(" + ") + s" $mode $constant"

  val simpleEvaluation = 3
}
