package concrete.constraint.semantic

import com.typesafe.scalalogging.LazyLogging

import concrete.Contradiction
import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.util.Interval

object SumMode extends Enumeration {
  type SumMode = Value
  val SumLE = Value("le")
  val SumLT = Value("lt")
  val SumEQ = Value("eq")
  //val SumNE = Value("ne")
}

final class Sum(
  val constant: Int,
  val factors: Array[Int],
  scope: Array[Variable],
  mode: SumMode.SumMode) extends Constraint(scope)
  with LazyLogging {

  import SumMode._

  require(factors.forall(_ != 0), this)

  def this(constant: Int, scope: Array[Variable], mode: SumMode.SumMode) =
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

  def advise(ps: ProblemState, p: Int) = arity

  private val initBound = Interval(-constant, -constant)

  private def filter(dom: Domain, itv: Interval, neg: Boolean): Domain = mode match {
    case SumLE => if (neg) dom.removeUntil(itv.lb) else dom.removeAfter(itv.ub)
    case SumLT => if (neg) dom.removeTo(itv.lb) else dom.removeFrom(itv.ub)
    case SumEQ => dom & itv
  }

  def revise(ps: ProblemState): Outcome = {

    var filtered = ps

    var bounds = initBound

    var i = arity - 1

    while (i >= 0) {
      bounds += ps.span(scope(i)) * factors(i)
      i -= 1
    }

    var change = true
    while (change) {
      change = false

      i = arity - 1
      while (i >= 0) {
        val dom = filtered.dom(scope(i))
        val f = factors(i)
        val myBounds = dom.span * f

        val thisBounds = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub)

        val newDom = filter(dom, thisBounds / -f, f < 0)

        if (newDom.isEmpty) {
          return Contradiction
        } else if (newDom ne dom) {
          filtered = filtered.updateDomNonEmpty(scope(i), newDom)
          change = true
          bounds = thisBounds + newDom.span * f
        }

        i -= 1
      }
    }
    filtered
  }

  override def toString(ps: ProblemState) =
    (scope, factors).zipped.map((v, f) => f + "." + v.toString(ps)).mkString(" + ") + s" $mode $constant"

  val simpleEvaluation = 3
}
