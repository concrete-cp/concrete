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
  mode: SumMode) extends Constraint(scope) with BC
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
    case SumLE if neg => dom.removeToVal(itv.lb - 1)
    case SumLE => dom.removeFromVal(itv.ub + 1)
    case SumEQ => dom.intersectVal(itv)
    //case SumNE => dom.removeValInterval(itv.lb, itv.ub)
  }

  def shave(): List[Int] = {

    var bounds = initBound

    var i = arity - 1

    while (i >= 0) {
      bounds += scope(i).dom.valueInterval * factors(i)
      i -= 1
    }

    //val bounds = domFact map { case (d, f) => d.valueInterval * f } reduce (_ + _)

    //reduce (_ + _)
    //    val bounds = Interval(0, 0)

    var ch: List[Int] = Nil
    i = arity - 1
    while (i >= 0) {
      val dom = scope(i).dom
      val f = factors(i)
      val myBounds = dom.valueInterval * f

      val boundsf = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub) / -f

      if (filter(dom, boundsf, f < 0)) {
        ch ::= i
      }
      i -= 1
    }
    ch
  }

  override def toString = (scope, factors).zipped.map((v, f) => f + "." + v).mkString(" + ") + s" $mode $constant"

  val simpleEvaluation = 3
}
