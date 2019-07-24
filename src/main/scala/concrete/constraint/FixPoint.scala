package concrete.constraint

import concrete._
import concrete.util.Interval

import scala.annotation.tailrec

trait FixPoint {
  @annotation.tailrec
  final def fixPoint(ps: ProblemState, shave: ProblemState => Outcome): Outcome = {
    shave(ps) match {
      case c: Contradiction =>
        c
      case ns: ProblemState =>
        if (ns.sameDomains(ps)) {
          ns
        } else {
          fixPoint(ns, shave)
        }
    }
  }
//
//  def fixPointM(ps: ProblemState, shavers: IndexedSeq[ProblemState => Outcome]): Outcome = {
//    fixPoint(ps, shavers.indices, (ps, i) => shavers(i)(ps))
//  }
//
  def fixPoint(ps: ProblemState, range: Range, shave: (ProblemState, Int) => Outcome): Outcome = {
    if (range.isEmpty) ps
    else {
      val it = Iterator.continually(range).flatten
      var i = it.next()
      var lastModified = i
      var state = shave(ps, i)

      i = it.next()
      while (i != lastModified && state.isState) {
        val ns = shave(state.toState, i)
        if (ns ne state) {
          lastModified = i
          state = ns
        }
        i = it.next()
      }
      state
    }
  }

}

trait OpsFixPoint extends Constraint {
  def domOps(doms: Array[Domain], pos: Int): Domain

  def fixPoint(ps: ProblemState): Outcome = {

    val doms = Array.tabulate(arity)(p => ps.dom(scope(p)))

    @tailrec
    def fixPoint(i: Int, last: Int): Option[Int] = {
      if (i < 0) {
        fixPoint(doms.length - 1, last)
      } else {
        // print(s"${doms.toSeq}, revising $i: ")
        val d = domOps(doms, i)
        if (d eq doms(i)) {
          if (i == last) {
            //println("End")
            None
          } else {
            fixPoint(i - 1, last)
          }
        } else if (d.isEmpty) {
          // println("Empty domain")
          Some(i)
        } else {
          doms(i) = d
          fixPoint(i - 1, i)
        }
      }

    }

    fixPoint(doms.length - 1, 0) match {
      case Some(i) => Contradiction(Seq(scope(i)))
      case None => ps.fold(0 until arity)((ps, p) => ps.updateDomNonEmpty(scope(p), doms(p)))
    }
  }
}

trait ItvFixPoint extends OpsFixPoint {
  override def domOps(doms: Array[Domain], pos: Int): Domain = {
    itvOps(doms, pos).map(doms(pos) & _).getOrElse(EmptyIntDomain)
  }

  def itvOps(doms: Array[Domain], i: Int): Option[Interval]
}

trait ItvArrayFixPoint extends ItvFixPoint {
  def ops: Array[Array[Domain] => Option[Interval]]
  def itvOps(doms: Array[Domain], i: Int): Option[Interval] = ops(i)(doms)
}