package concrete.constraint.extension

import concrete.Contradiction
import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Removals
import concrete.util.SparseSet

/* MDDRelation comes with its own timestamp */
class MDDC(_scope: Array[Variable], private val mdd: MDDRelation)
  extends Constraint(_scope) with Removals {

  def initState = new SparseSet(mdd.identify + 1)

  // Members declared in concrete.constraint.Constraint
  override def check(t: Array[Int]) = mdd.contains(t)

  def checkValues(tuple: Array[Int]): Boolean = throw new UnsupportedOperationException

  val simpleEvaluation: Int = math.min(Constraint.NP, scope.count(_.initDomain.length > 1))

  // Members declared in concrete.constraint.Removals
  val prop = mdd.edges.toDouble / scope.map(_.initDomain.size.toDouble).product

  def getEvaluation(ps: ProblemState) = (prop * doubleCardSize(ps)).toInt

  private val unsupported = scope.map(p => new collection.mutable.BitSet(p.initDomain.size))

  var delta: Int = _

  var gNo: Set[Int] = _

  def revise(ps: ProblemState, modified: List[Int]) = {

    val oldGno: Set[Int] = ps(this)
    val domains = ps.domains(scope).toArray
    val unsupported = domains.map(_.to[collection.mutable.Set])

    delta = arity

    this.gNo = oldGno

    val sat = seekSupports(domains, mdd.timestamp.next(), mdd.mdd, 0)
    if (!sat) {
      Contradiction
    } else {
      var cs: Outcome = ps.updateState(id, gNo)
      for (p <- 0 until delta) {
        cs = cs.filterDom(p)(!unsupported(p)(_))
      }
      cs.entailIfFree(this)
    }
  }

  private def seekSupports(domains: Array[Domain], ts: Int, g: MDD, i: Int): Boolean = {
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

        if (seekSupports(domains, ts, gk, i + 1)) {
          res = true
          unsupported(i) -= ak

          if (i + 1 == delta && unsupported(i).isEmpty) {
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
