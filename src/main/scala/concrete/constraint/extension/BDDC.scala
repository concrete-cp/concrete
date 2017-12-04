package concrete.constraint.extension

import java.util

import bitvectors.BitVector
import concrete._
import concrete.constraint.{Constraint, StatefulConstraint}
import concrete.util.SparseSet
import mdd._

/* MDDRelation comes with its own timestamp */
class BDDC(_scope: Array[Variable], val bdd: BDDRelation)
  extends Constraint(_scope) with StatefulConstraint[SparseSet] {

  val simpleEvaluation: Int = math.min(Constraint.NP, scope.count(_.initDomain.size > 1))
  // Members declared in concrete.constraint.Removals
  val prop: Double = bdd.edges.toDouble / scope.map(_.initDomain.size.toDouble).product

  override def init(ps: ProblemState): Outcome = {
    val max = bdd.bdd.fastIdentify() + 1
    //println(s"********** $max **********")
    ps.updateState(this, new SparseSet(max))
  }

  // Members declared in concrete.constraint.Constraint
  override def check(t: Array[Int]): Boolean = bdd.contains(t)

  def checkValues(tuple: Array[Int]): Boolean = throw new UnsupportedOperationException

  def advise(ps: ProblemState, event: Event, pos: Int): Int = (prop * doubleCardSize(ps)).toInt

  def revise(ps: ProblemState, modified: BitVector): Outcome = {

    val oldGno = ps(this)
    val domains = Array.tabulate(arity)(p => ps.dom(scope(p)))
    val supported = Array.fill(arity)(new util.HashSet[Int]())

    // val unsupported = domains.map(_.to[collection.mutable.Set])

    var delta = arity

    var gNo = oldGno //.clone()

    val gYes = new TSSet[BDD] // SparseSet()

    @inline
    def seekSupports(g: BDD, i: Int): Boolean = {
      if (g eq BDDLeaf) {
        if (i < delta) {
          delta = i
        }
        true
      } else if (g eq BDD0) {
        false
      } else if (gYes.contains(g)) {
        true
      } else if (gNo.contains(g.id)) {
        false
      } else if (domains(i).present(g.index) && seekSupports(g.child, i + 1)) {
        supported(i).add(g.index)
        if (i + 1 == delta && supported(i).size == domains(i).size) {
          delta = i
        } else {
          seekSupports(g.sibling, i)
        }
        gYes.put(g)
        true
      } else if (seekSupports(g.sibling, i)) {
        gYes.put(g)
        true
      } else {
        gNo += g.id
        false
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
          cs = cs.updateDomNonEmptyNoCheck(scope(p), domains(p).filter(supported(p).contains))
        }
      }
      cs.entailIfFree(this)
    }
  }

  override def dataSize = bdd.edges

}
