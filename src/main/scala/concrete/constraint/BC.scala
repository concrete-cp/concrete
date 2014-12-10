package concrete.constraint

import scala.annotation.tailrec
import concrete.ReviseOutcome
import concrete.Contradiction
import concrete.Revised
import concrete.Domain

trait BC extends Constraint {
  def shave(domains: IndexedSeq[Domain], state: State): ReviseOutcome[State]

  override final def revise(domains: IndexedSeq[Domain], state: State): ReviseOutcome[State] = {
    var c = true
    var cs = state
    var cd = domains
    while (c) {
      c = false
      shave(cd, cs) match {
        case Contradiction => return Contradiction
        case Revised(nd, entailed, ns) =>
          if (entailed) {
            return Revised(nd, true, ns)
          } else {
            for (i <- 0 until arity) {
              val ndi = nd(i)
              if (ndi.isEmpty) {
                return Contradiction
              } else {
                c |= (cd(i) ne ndi)
              }
            }

            cd = nd
            cs = ns

          }

      }

    }
    Revised(cd, false, cs)

  }
}

//trait StatelessBC extends BC {
//
//  type State = Unit
//
//  def initState: State = Unit
//
//  def shave(domains: IndexedSeq[Domain], state: State) = shave(domains)
//
//  def shave(domains: IndexedSeq[Domain]): ReviseOutcome[State]
//
//  def toString(domains: IndexedSeq[Domain]): String = this.getClass.getSimpleName +
//    (scope, domains).zipped.map((v, d) => s"$v $d").mkString("(", ", ", ")")
//
//  override def toString(domains: IndexedSeq[Domain], s: State) = toString(domains)
//
//  def isConsistent(domains: IndexedSeq[Domain]): Boolean = revise(domains, Unit) match {
//    case Contradiction      => false
//    case Revised(mod, _, _) => mod.forall(_.nonEmpty)
//  }
//
//  override final def isConsistent(domains: IndexedSeq[Domain], s: State): Boolean = isConsistent(domains)
//
//}
