package concrete.constraint.semantic;

import scala.annotation.tailrec
import concrete.constraint.Constraint
import concrete.Domain
import concrete.util.Interval
import concrete.Variable
import cspom.Loggable
import concrete.constraint.Shaver
import scala.collection.mutable.HashSet

object FilterSum extends Enumeration {
  type FilterSum = Value
  val SumLE = Value("le")
  val SumLT = Value("lt")
  val SumGE = Value("ge")
  val SumGT = Value("gt")
  val SumEQ = Value("eq")
  val SumNE = Value("ne")
}

import FilterSum._

final class Sum(
  val constant: Int,
  val factors: Array[Int],
  scope: Array[Variable],
  mode: FilterSum) extends Constraint(scope)
  with Loggable {

  def this(constant: Int, scope: Array[Variable], mode: FilterSum) =
    this(constant, Array.fill(scope.length)(1), scope, mode)

  //val domFact = scope map (_.dom) zip factors toList

  def checkValues(t: Array[Int]): Boolean =
    (0 until arity).map(i => t(i) * factors(i)).sum == constant

  def advise(p: Int) = arity

  val initBound = Interval(-constant, -constant)

  private def filter(dom: Domain, itv: Interval, neg: Boolean): Boolean = mode match {
    case SumGE if neg => dom.removeFromVal(itv.ub + 1)
    case SumGE => dom.removeToVal(itv.lb - 1)
    case SumLE if neg => dom.removeToVal(itv.lb - 1)
    case SumLE => dom.removeFromVal(itv.ub + 1)
    case SumEQ => dom.intersectVal(itv)
    case SumNE => dom.removeValInterval(itv.lb, itv.ub)
    case _ => ???
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

  def revise() = {
    var mod = new HashSet[Int]()
    var ch = true
    while (ch) {
      ch = false
      val m = shave()
      if (m.nonEmpty) {
        ch = true
        mod ++= m
      }
    }
    mod
  }

  def reviseVariable(p: Int, mod: Seq[Int]) = false

  override def toString = (scope, factors).zipped.map((v, f) => f + "." + v).mkString(" + ") + " = " + constant
  val simpleEvaluation = 3
}
