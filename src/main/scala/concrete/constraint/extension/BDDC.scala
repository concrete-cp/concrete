package concrete.constraint.extension

import bitvectors.BitVector
import concrete._
import concrete.constraint.{Constraint, StatefulConstraint}
import concrete.util.SparseSet
import mdd._

/* MDDRelation comes with its own timestamp */
final class BDDC(_scope: Array[Variable], val relation: BDDRelation)
  extends Constraint(_scope) with StatefulConstraint[SparseSet] {

  val simpleEvaluation: Int = math.min(Constraint.NP, scope.count(_.initDomain.size > 1))
  // Members declared in concrete.constraint.Removals
  val prop: Double = relation.edges.toDouble / scope.map(_.initDomain.size.toDouble).product

  override def init(ps: ProblemState): Outcome = {
    val max = relation.bdd.fastIdentify() + 1
    //println(s"********** $max **********")
    ps.updateState(this, new SparseSet(max))
  }

  // Members declared in concrete.constraint.Constraint
  override def check(t: Array[Int]): Boolean = relation.contains(t)

  def checkValues(tuple: Array[Int]): Boolean = throw new UnsupportedOperationException

  def advise(ps: ProblemState, event: Event, pos: Int): Int = (prop * doubleCardSize(ps)).toInt

  def revise(ps: ProblemState, modified: BitVector): Outcome = {

    val oldGno = ps(this)
    val domains = Array.tabulate(arity)(p => ps.dom(scope(p)))
    val dSizes = domains.map(_.size)
    val supported = domains.map(d => new IntDomainBuilder(d.head, 1 + d.last - d.head))
    val cardinalities = Array.fill(arity)(0)

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
      } else if (domains(i).contains(g.index) && seekSupports(g.child, i + 1)) {
        if (!supported(i)(g.index)) {
          supported(i) += g.index
          cardinalities(i) += 1
        }
        if (i + 1 == delta && cardinalities(i) == dSizes(i)) {
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
        gNo = gNo.incl(g.id)
        false
      }

    }

    val sat = seekSupports(relation.bdd, 0)
    if (!sat) {
      Contradiction(scope)
    } else {
      var cs: ProblemState =
        if (gNo.size == oldGno.size) ps else ps.updateState(this, gNo)
      for (p <- 0 until delta) {
        val newSize = cardinalities(p)
        if (newSize < domains(p).size) {
          cs = cs.updateDomNonEmptyNoCheck(scope(p), supported(p).result(newSize))
        }
      }
      cs.entailIfFree(this)
    }
  }

  override def dataSize: Int = relation.edges

}
