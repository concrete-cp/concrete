package concrete.constraint.extension

import bitvectors.BitVector
import concrete._
import concrete.constraint.{Constraint, Removals, StatefulConstraint}
import concrete.util.SparseSet
import mdd.{BDD, BDD0, BDDLeaf, BDDNode}

/* MDDRelation comes with its own timestamp */
class BDDC(_scope: Array[Variable], val bdd: BDDRelation)
  extends Constraint(_scope) with Removals with StatefulConstraint[SparseSet] {

  val simpleEvaluation: Int = math.min(Constraint.NP, scope.count(_.initDomain.size > 1))
  // Members declared in concrete.constraint.Removals
  val prop = bdd.edges.toDouble / scope.map(_.initDomain.size.toDouble).product

  override def init(ps: ProblemState) = {
    val max = bdd.bdd.identify() + 1
    //println(s"********** $max **********")
    ps.updateState(this, new SparseSet(max))
  }

  // Members declared in concrete.constraint.Constraint
  override def check(t: Array[Int]) = bdd.contains(t)

  def checkValues(tuple: Array[Int]): Boolean = throw new UnsupportedOperationException

  def getEvaluation(ps: ProblemState) = (prop * doubleCardSize(ps)).toInt

  def revise(ps: ProblemState, modified: BitVector) = {

    val oldGno = ps(this)
    val domains = Array.tabulate(arity)(p => ps.dom(scope(p)))
    val supported = Array.fill[IntDomain](arity)(EmptyIntDomain)

    // val unsupported = domains.map(_.to[collection.mutable.Set])

    var delta = arity

    var gNo = oldGno //.clone()

    var gYes = new SparseSet(gNo.capacity)

    @inline
    def seekSupports(g: BDD, i: Int): Boolean = {
      g match {
        case BDDLeaf =>
          if (i < delta) {
            delta = i
          }
          true
        case BDD0 => false
        case ng: BDDNode =>

          if (gYes.contains(ng.id)) {
            true
          } else if (gNo.contains(ng.id)) {
            false
          } else if (domains(i).present(ng.index) && seekSupports(ng.child, i + 1)) {
            supported(i) |= ng.index
            if (i + 1 == delta && supported(i).size == domains(i).size) {
              delta = i
            } else {
              seekSupports(ng.sibling, i)
            }
            gYes += g.id
            true
          } else if (seekSupports(ng.sibling, i)) {
            gYes += g.id
            true
          } else {
            gNo += g.id
            false
          }
      }
    }

    val sat = seekSupports(bdd.bdd, 0)
    if (!sat) {
      Contradiction(scope)
    } else {
      var cs: ProblemState =
        if (gNo.size == oldGno.size) ps else ps.updateState(this, gNo)
      for (p <- 0 until delta) {
        if (supported(p).size < domains(p).size) {
          cs = cs.updateDomNonEmptyNoCheck(scope(p), supported(p))
        }
      }
      cs.entailIfFree(this)
    }
  }

  override def dataSize = bdd.edges

}
