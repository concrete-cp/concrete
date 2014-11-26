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
import concrete.constraint.StatelessBC
import concrete.constraint.Stateless
import concrete.Revised

final class Sum(
  val constant: Int,
  val factors: Array[Int],
  scope: Array[Variable],
  mode: SumMode) extends Constraint(scope)
  with Stateless
  with LazyLogging {

  require(factors.forall(_ != 0), this)

  def this(constant: Int, scope: Array[Variable], mode: SumMode) =
    this(constant, Array.fill(scope.length)(1), scope, mode)

  def check(t: Array[Int]): Boolean = {
    val sum = (0 until arity).map(i => t(i) * factors(i)).sum
    mode match {
      case SumLE => sum <= constant
      case SumLT => sum < constant
      case SumEQ => sum == constant
      // case SumNE => sum != constant
    }
  }

  def advise(domains: IndexedSeq[Domain],p: Int) = arity

  private val initBound = Interval(-constant, -constant)

  private def filter(dom: Domain, itv: Interval, neg: Boolean): Domain = mode match {
    case SumLE => if (neg) dom.removeUntil(itv.lb) else dom.removeAfter(itv.ub)
    case SumLT => if (neg) dom.removeTo(itv.lb) else dom.removeFrom(itv.ub)
    case SumEQ => dom & itv
  }

  def revise(domains: IndexedSeq[Domain]) = {

    val doms = domains.toArray.clone

    var bounds = initBound

    var i = arity - 1

    while (i >= 0) {
      bounds += doms(i).span * factors(i)
      i -= 1
    }

    var change = true
    while (change) {
      change = false

      i = arity - 1
      while (i >= 0) {
        val dom = doms(i)
        val f = factors(i)
        val myBounds = dom.span * f

        val thisBounds = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub)

        val newDom = filter(dom, thisBounds / -f, f < 0)

        if (newDom ne dom) {
          doms(i) = newDom
          change = true
          bounds = thisBounds + newDom.span * f
        }

        i -= 1
      }
    }
    Revised(doms)
  }

  override def toString(domains: IndexedSeq[Domain]) = (domains, factors).zipped.map((v, f) => f + "." + v).mkString(" + ") + s" $mode $constant"

  val simpleEvaluation = 3
}
