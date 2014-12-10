package concrete.constraint.semantic;

import concrete.Contradiction
import concrete.Domain
import concrete.Revised
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.BC
import concrete.EmptyIntDomain
import concrete.util.Interval

//object AbsDiffBC {
//  def unionInter(dom: Domain, i0: Interval, j0: Interval, i1: Interval, j1: Interval) = {
//    val k0 = i0 intersect j0
//    val k1 = i1 intersect j1
//
//    (k0, k1) match {
//      case (None, None)    => throw UNSATObject
//      case (None, Some(k)) => dom & k
//      case (Some(k), None) => dom & k.intersect(k)
//      case (Some(k0), Some(k1)) =>
//        val d1 = dom.intersect(k0 span k1)
//
//        if (k0.ub < k1.lb) d1.filter(v => k0.ub >= v || v >= k1.lb)
//        else if (k1.ub < k0.lb) d1.filter(v => k1.ub >= v || v >= k0.lb)
//        else d1
//    }
//  }
//}

final class AbsDiffBC(val result: Variable, val v0: Variable, val v1: Variable)
  extends Constraint(Array(result, v0, v1)) with BC {
  type State = Unit

  def initState = Unit
  def check(t: Array[Int]) = t(0) == math.abs(t(1) - t(2))

  def union(i0: Option[Interval], i1: Option[Interval]): Option[Interval] = {
    i0.map {
      i: Interval =>
        if (i1.isDefined) {
          i span i1.get
        } else {
          i
        }
    }
      .orElse(i1)
  }

  def shave(domains: IndexedSeq[Domain], s: State) = {
    val d2span = domains(2).span
    val d1span = domains(1).span
    val r = domains(0) & (d1span - d2span).abs
    if (r.isEmpty) {
      Contradiction
    } else {
      val rspan = r.span

      union(d1span.intersect(d2span - rspan), d1span.intersect(d2span + rspan)) match {
        case None => Contradiction
        case Some(f) =>
          val i0 = domains(1) & f

          if (i0.isEmpty) {
            Contradiction
          } else {

            val d1span = i0.span
            union(
              d2span.intersect(d1span - rspan),
              d2span.intersect(d1span + rspan)) match {
                case None => Contradiction
                case Some(g) =>
                  Revised(IndexedSeq(r, i0, domains(2) & g))
              }

          }
      }
    }

  }

  override def toString(domains: IndexedSeq[Domain], s: State) =
    s"$result ${domains(0)} =BC= |$v0 ${domains(1)} - $v1 ${domains(2)}|";

  def advise(domains: IndexedSeq[Domain], pos: Int) = 5

  def simpleEvaluation = 2
}
