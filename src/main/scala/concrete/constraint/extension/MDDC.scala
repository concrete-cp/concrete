package concrete.constraint.extension

import concrete.Contradiction
import concrete.Domain
import concrete.EmptyIntDomain
import concrete.IntDomain
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Removals
import concrete.constraint.StatefulConstraint
import concrete.util.SparseSet
import bitvectors.BitVector

/* MDDRelation comes with its own timestamp */
class MDDC(_scope: Array[Variable], val mdd: MDDRelation)
    extends Constraint(_scope) with Removals with StatefulConstraint[SparseSet] {

  override def init(ps: ProblemState) = {
    val max = mdd.identify() + 1
    ps.updateState(this, new SparseSet(max)) //new SparseSet(max))
  }

  // Members declared in concrete.constraint.Constraint
  override def check(t: Array[Int]) = mdd.contains(t)

  def checkValues(tuple: Array[Int]): Boolean = throw new UnsupportedOperationException

  val simpleEvaluation: Int = math.min(Constraint.NP, scope.count(_.initDomain.size > 1))

  // Members declared in concrete.constraint.Removals
  val prop = mdd.edges.toDouble / scope.map(_.initDomain.size.toDouble).product

  def getEvaluation(ps: ProblemState) = (prop * doubleCardSize(ps)).toInt

  def revise(ps: ProblemState, modified: BitVector) = {

    val domains = ps.doms(scope) //Array.tabulate(arity)(p => ps.dom(scope(p)))
    val supported = Array.fill[IntDomain](arity)(EmptyIntDomain)

    // val unsupported = domains.map(_.to[collection.mutable.Set])

    var delta = arity

    var gNo = ps(this) //.clone()

    var gNoChange = false

    val ts = mdd.timestamp.next

    def seekSupports(g: MDD, i: Int): Boolean = {

      @inline
      def loop(dom: Domain): Boolean = {
        var res = false
        for (ak <- dom) {
          val gk = g.subMDD(ak)

          if (seekSupports(gk, i + 1)) {
            res = true
            supported(i) |= ak
            if (i + 1 == delta && supported(i).size == domains(i).size) {
              delta = i
              return true
            }
          }

        }

        res

      }

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
      } else if (loop(domains(i))) {
        g.cache.timestamp = ts
        true
      } else {
        gNo += g.id
        gNoChange = true
        false
      }

    }

    val sat = seekSupports(mdd.mdd, 0)
    if (sat) {
      var cs: ProblemState =
        if (gNoChange) ps.updateState(this, gNo) else ps
      for (p <- 0 until delta) {
        if (supported(p).size < domains(p).size) {
          cs = cs.updateDomNonEmptyNoCheck(scope(p), supported(p))
        }
      }
      cs.entailIfFree(this)
    } else {
      Contradiction(scope)
    }
  }

  override def dataSize = mdd.edges

}
