package concrete.constraint.extension

import concrete.Contradiction
import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Removals
import concrete.constraint.StatefulConstraint
import concrete.util.SparseSet
import concrete.EmptyIntDomain
import concrete.IntDomain

/* MDDRelation comes with its own timestamp */
class MDDC(_scope: Array[Variable], private val mdd: MDDRelation)
    extends Constraint(_scope) with Removals with StatefulConstraint[SparseSet] {

  override def init(ps: ProblemState) = ps.updateState(this, new SparseSet(mdd.identify + 1))

  // Members declared in concrete.constraint.Constraint
  override def check(t: Array[Int]) = mdd.contains(t)

  def checkValues(tuple: Array[Int]): Boolean = throw new UnsupportedOperationException

  val simpleEvaluation: Int = math.min(Constraint.NP, scope.count(_.initDomain.length > 1))

  // Members declared in concrete.constraint.Removals
  val prop = mdd.edges.toDouble / scope.map(_.initDomain.size.toDouble).product

  def getEvaluation(ps: ProblemState) = (prop * doubleCardSize(ps)).toInt

  var delta: Int = _

  var gNo: SparseSet = _

  def revise(ps: ProblemState, modified: Seq[Int]) = {

    val oldGno = ps(this)
    val domains = Array.tabulate(arity)(p => ps.dom(scope(p)))
    val supported = Array.fill[IntDomain](arity)(EmptyIntDomain)

    // val unsupported = domains.map(_.to[collection.mutable.Set])

    delta = arity

    this.gNo = oldGno

    val sat = seekSupports(domains, supported, mdd.timestamp.next(), mdd.mdd, 0)
    if (!sat) {
      Contradiction
    } else {
      var cs: ProblemState = ps.updateState(this, gNo)
      for (p <- 0 until delta) {
        cs = cs.updateDomNonEmpty(scope(p), supported(p))
      }
      cs.entailIfFree(this)
    }
  }

  private def seekSupports(domains: Array[Domain], supported: Array[IntDomain], ts: Int, g: MDD, i: Int): Boolean = {
    if (g eq MDDLeaf) {
      if (i < delta) {
        delta = i
      }
      true
    } else if (g eq MDD0) {
      false
    } else if (g.cache.timestamp == ts) {
      true
    } else if (gNo.contains(g.id)) {
      false
    } else {
      var res = false
      val dom = domains(i)
      var ak = dom.head
      var continue = true
      while (continue) {
        val gk = g.subMDD(ak)

        if (seekSupports(domains, supported, ts, gk, i + 1)) {
          res = true
          supported(i) |= ak
          // unsupported(i) -= ak

          if (i + 1 == delta && supported(i).length == domains(i).length) {
            delta = i
            continue = false
          }
        }

        if (ak == dom.last) {
          continue = false
        } else {
          ak = dom.next(ak)
        }
      }
      if (res) {
        g.cache.timestamp = ts
      } else {
        gNo += g.id
      }
      res
    }
  }

  override def dataSize = mdd.edges

}
