package concrete.constraint.semantic;

import concrete.Domain
import concrete.Revised
import concrete.UNSATObject
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.StatelessBC
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
  extends Constraint(Array(result, v0, v1)) with StatelessBC {

  def check(t: Array[Int]) = t(0) == math.abs(t(1) - t(2))

  def shave(domains: IndexedSeq[Domain]) = {

    val r = domains(0) & (domains(1).span - domains(2).span).abs
    val rspan = r.span
    val d2span = domains(2).span
    val i0 = domains(1) & ((d2span - rspan) span (d2span + rspan))
    val d1span = i0.span
    val i1 = domains(2) & ((d1span - rspan) span (d1span + rspan))

    Revised(Array(r, i0, i1))

    //    val i0 = v0.dom.valueInterval
    //    val i1 = v1.dom.valueInterval
    //
    //    val diff = i0 - i1
    //
    //    var mod: List[Int] = Nil
    //    if (result.dom.intersectVal(diff.abs)) {
    //      mod ::= 0
    //    }
    //    val r = result.dom.valueInterval
    //
    //    if (diff.lb >= 0) {
    //      if (v0.dom.intersectVal(i1 + r)) {
    //        mod ::= 1
    //      }
    //      if (v1.dom.intersectVal(i0 - r)) {
    //        mod ::= 2
    //      }
    //    } else if (diff.ub <= 0) {
    //      if (v0.dom.intersectVal(i1 - r)) {
    //        mod ::= 1
    //      }
    //      if (v1.dom.intersectVal(i0 + r)) {
    //        mod ::= 2
    //      }
    //    } else {
    //      if (AbsDiffBC.unionInter(v0.dom, i0, i1 + r, i0, i1 - r)) {
    //        mod ::= 1
    //      }
    //      if (AbsDiffBC.unionInter(v1.dom, i1, i0 - r, i1, i0 + r)) {
    //        mod ::= 2
    //      }
    //    }

    //Revised(mod, false)
  }

  override def toString(domains:IndexedSeq[Domain]) = domains(0) + " =BC= |" + domains(1) + " - " + domains(2) + "|";

  def advise(domains: IndexedSeq[Domain], pos: Int) = 5

  def simpleEvaluation = 2
}
