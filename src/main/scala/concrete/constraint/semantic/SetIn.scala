package concrete
package constraint
package semantic

object SetIn {
  def apply(r: Variable, v: Variable, c: Seq[Int]): SetIn =
    new SetIn(r, v, IntDomain.ofSeq(c: _*))
}
class SetIn(val r: Variable, val v: Variable, val c: Domain)
    extends Constraint(Array(r, v)) {

  def advise(problemState: ProblemState, event: Event, position: Int): Int = c.size
  def init(ps: concrete.ProblemState): Outcome = ps
  def simpleEvaluation: Int = 1

  def check(t: Array[Int]) = t(0) match {
    case 1 => c.present(t(1))
    case 0 => !c.present(t(1))
    case _ => sys.error(s"${t(0)} must be boolean")
  }

  def revise(ps: ProblemState): Outcome = {
    ps.dom(r) match {
      case BooleanDomain.UNKNOWNBoolean =>
        val vd = ps.dom(v)
        if (vd disjoint c) {
          ps.assign(r, 0).entail(this)
        } else if (vd subsetOf c) {
          ps.assign(r, 1).entail(this)
        } else {
          ps
        }
      case BooleanDomain.TRUE => ps.intersectDom(v, c).entail(this)
      case BooleanDomain.FALSE => ps.filterDom(v)(!c.present(_)).entail(this)
    }
  }

  override def toString(ps: ProblemState) = s"${r.toString(ps)} = (${v.toString(ps)} in $c)"

}