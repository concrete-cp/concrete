package concrete.constraint.semantic

import com.typesafe.scalalogging.LazyLogging
import concrete.Contradiction
import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.util.Interval
import concrete.constraint.ScopeIds

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
  mode: SumMode.SumMode) extends Constraint(scope) with LazyLogging {

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

  private def filter(span: Interval, itv: Interval, neg: Boolean): Option[Interval] = mode match {
    case SumLE => if (neg) span.shaveLb(itv.lb) else span.shaveUb(itv.ub)
    case SumLT => if (neg) span.shaveLb(itv.lb + 1) else span.shaveUb(itv.ub - 1)
    case SumEQ => span.intersect(itv)
  }

  val doms = new Array[Interval](arity)

  def revise(ps: ProblemState): Outcome = {

    var bounds = initBound

    var i = arity - 1
    while (i >= 0) {
      val d = ps.dom(scope(i)).span
      doms(i) = d
      bounds += d * factors(i)
      i -= 1
    }

    var change = true
    while (change) {
      change = false

      var i = 0
      while (i >= 0) {
        val span = doms(i)
        val f = factors(i)

        val myBounds = span * f

        val thisBounds = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub)

        filter(span, thisBounds / -f, f < 0) match {
          case None => return Contradiction
          case Some(newSpan) => if (newSpan ne span) {
            change = true
            bounds = thisBounds + newSpan * f
            doms(i) = newSpan
          }
        }

        i -= 1
      }

    }

    var filtered = ps
    if (change) {
      var i = arity - 1
      while (i >= 0) {
        filtered = filtered.shaveDomNonEmpty(scope(i), doms(i))
        i -= 1
      }
    }
    filtered
  }

  override def toString(ps: ProblemState) =
    (scope, factors).zipped.map((v, f) => f + "." + v.toString(ps)).mkString(" + ") + s" $mode $constant"

  val simpleEvaluation = 3
}
