package concrete.constraint.extension

import concrete.Contradiction
import concrete.EmptyIntDomain
import concrete.IntDomain
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Removals
import concrete.constraint.StatefulConstraint
import concrete.util.SparseSet

/* MDDRelation comes with its own timestamp */
class BDDC(_scope: Array[Variable], val bdd: BDDRelation)
    extends Constraint(_scope) with Removals with StatefulConstraint[SparseSet] {

  override def init(ps: ProblemState) = {
    val max = bdd.identify() + 1
    //println(s"********** $max **********")
    ps.updateState(this, new SparseSet(max))
  }

  // Members declared in concrete.constraint.Constraint
  override def check(t: Array[Int]) = bdd.contains(t)

  def checkValues(tuple: Array[Int]): Boolean = throw new UnsupportedOperationException

  val simpleEvaluation: Int = math.min(Constraint.NP, scope.count(_.initDomain.length > 1))

  // Members declared in concrete.constraint.Removals
  val prop = bdd.edges.toDouble / scope.map(_.initDomain.size.toDouble).product

  def getEvaluation(ps: ProblemState) = (prop * doubleCardSize(ps)).toInt

  def revise(ps: ProblemState, modified: Seq[Int]) = {

    val oldGno = ps(this)
    val domains = Array.tabulate(arity)(p => ps.dom(scope(p)))
    val supported = Array.fill[IntDomain](arity)(EmptyIntDomain)

    // val unsupported = domains.map(_.to[collection.mutable.Set])

    var delta = arity

    var gNo = oldGno //.clone()

    val ts = bdd.timestamp.next

    @inline
    def seekSupports(g: BDD, i: Int): Boolean = {
      if (g eq BDDLeaf) {
        if (i < delta) {
          delta = i
        }
        true
      } else if (g eq BDD0) {
        false
      } else {
        val ng = g.asInstanceOf[BDDNode]
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
          gNo += g.id
          false
        }
      }
    }

    val sat = seekSupports(bdd.bdd, 0)
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

  override def dataSize = bdd.edges

}
