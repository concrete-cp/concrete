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
import scala.collection.mutable.BitSet
import cspom.util.BitVector

/* MDDRelation comes with its own timestamp */
class MDDCLink(_scope: Array[Variable], val mdd: BDDRelation)
    extends Constraint(_scope) with Removals with StatefulConstraint[BitSet] {

  override def init(ps: ProblemState) = {
    val max = mdd.identify() + 1
    ps.updateState(this, new BitSet(max)) //new SparseSet(mdd.identify + 1))
  }

  // Members declared in concrete.constraint.Constraint
  override def check(t: Array[Int]) = mdd.contains(t)

  def checkValues(tuple: Array[Int]): Boolean = throw new UnsupportedOperationException

  val simpleEvaluation: Int = math.min(Constraint.NP, scope.count(_.initDomain.length > 1))

  // Members declared in concrete.constraint.Removals
  val prop = mdd.edges.toDouble / scope.map(_.initDomain.size.toDouble).product

  def getEvaluation(ps: ProblemState) = (prop * doubleCardSize(ps)).toInt

  def revise(ps: ProblemState, modified: Seq[Int]) = {

    val oldGno = ps(this)
    val domains = Array.tabulate(arity)(p => ps.dom(scope(p)))
    val supported = Array.fill[IntDomain](arity)(EmptyIntDomain)

    // val unsupported = domains.map(_.to[collection.mutable.Set])

    var delta = arity

    var gNo = oldGno.clone()

    val ts = mdd.timestamp.next

    @inline
    def seekSupports(g: BDD, i: Int): Boolean = {
      if (g eq MDDLinkLeaf) {
        if (i < delta) {
          delta = i
        }
        true
      } else if (g eq MDDLink0) {
        false
      } else {
        val ng = g.asInstanceOf[MDDLinkNode]
        if (ng.cache.timestamp == ts) {
          true
        } else if (gNo.contains(g.id)) {
          false
        } else if (domains(i).present(ng.index) && seekSupports(ng.child, i + 1)) {
          supported(i) |= ng.index
          if (i + 1 == delta && supported(i).length == domains(i).length) {
            delta = i
          } else {
            seekSupports(ng.sibling, i)
          }
          ng.cache.timestamp = ts
          true
        } else if (seekSupports(ng.sibling, i)) {
          ng.cache.timestamp = ts
          true
        } else {
          gNo.add(g.id)
          false
        }
      }
    }

    val sat = seekSupports(mdd.bdd, 0)
    if (!sat) {
      Contradiction
    } else {
      var cs: ProblemState =
        if (gNo.size == oldGno.size) ps else ps.updateState(this, gNo)
      for (p <- 0 until delta) {
        if (supported(p).length < domains(p).length) {
          cs = cs.updateDomNonEmptyNoCheck(scope(p), supported(p))
        }
      }
      cs.entailIfFree(this)
    }
  }

  override def dataSize = mdd.edges

}
