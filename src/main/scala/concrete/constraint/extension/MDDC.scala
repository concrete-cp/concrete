package concrete.constraint.extension

import java.util

import bitvectors.BitVector
import concrete._
import concrete.constraint.{Constraint, StatefulConstraint}
import concrete.util.SparseSet
import mdd.{MDD, MDD0, MDDLeaf, TSSet}

/* MDDRelation comes with its own timestamp */
class MDDC(_scope: Array[Variable], val mdd: MDDRelation)
  extends Constraint(_scope) with StatefulConstraint[SparseSet] {

  val simpleEvaluation: Int = math.min(Constraint.NP, scope.count(_.initDomain.size > 1))

  private val prop = mdd.edges.toDouble / scope.map(_.initDomain.size.toDouble).product

  override def init(ps: ProblemState): ProblemState = {
    val max = mdd.mdd.fastIdentify() + 1
    ps.updateState(this, new SparseSet(max)) //new SparseSet(max))
  }

  // Members declared in concrete.constraint.Constraint
  override def check(t: Array[Int]) = mdd.contains(t)

  def advise(ps: ProblemState, event: Event, pos: Int): Int = (prop * doubleCardSize(ps)).toInt

  def revise(ps: ProblemState, mod: BitVector): Outcome = {

    val domains = ps.doms(scope) //Array.tabulate(arity)(p => ps.dom(scope(p)))
    val supported = Array.fill(arity)(new util.HashSet[Int]())

    // val unsupported = domains.map(_.to[collection.mutable.Set])

    var delta = arity

    var gNo = ps(this) //.clone()

    var gNoChange = false

    val gYes = new TSSet[MDD]()

    //    if (mdd.lambda < 50) {
    //      mdd.map(_.mkString(", ")).foreach(println)
    //    }

    def seekSupports(g: MDD, i: Int): Boolean = {

      @inline
      def loop(dom: Domain): Boolean = {
        var res = false
        for (ak <- dom) {

          val gk = g.subMDD(ak)

          if (seekSupports(gk, i + 1)) {
            res = true
            supported(i).add(ak)
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
      } else if (gYes.contains(g)) {
        true
      } else if (gNo.contains(g.id)) {
        false
      } else if (loop(domains(i))) {
        gYes.put(g)
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
          cs = cs.updateDomNonEmptyNoCheck(scope(p), domains(p).filter(supported(p).contains))
        }
      }
      cs.entailIfFree(this)
    } else {
      Contradiction(scope)
    }
  }

  override def dataSize: Int = mdd.edges

}
