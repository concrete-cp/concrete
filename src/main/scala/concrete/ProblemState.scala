package concrete

import bitvectors.BitVector
import com.typesafe.scalalogging.LazyLogging
import concrete.constraint.{Constraint, StatefulConstraint}
import concrete.util.{IdentityMap, Interval}
import cspom.UNSATException

sealed trait Outcome {
  def andThen(f: ProblemState => Outcome): Outcome

  def orElse[A >: ProblemState](f: => A): A

  def map[A](f: ProblemState => A): Option[A]

  def filterDom(v: Variable)(f: Int => Boolean): Outcome

  def shaveDom(v: Variable, lb: Int, ub: Int): Outcome

  def shaveDom(v: Variable, itv: Interval): Outcome = shaveDom(v, itv.lb, itv.ub)

  def intersectDom(v: Variable, d: Domain): Outcome = {
    updateDom(v, dom(v) & d)
  }

  def removeTo(v: Variable, ub: Int): Outcome

  def removeFrom(v: Variable, lb: Int): Outcome

  def removeUntil(v: Variable, ub: Int): Outcome

  def removeAfter(v: Variable, lb: Int): Outcome

  def entail(c: Constraint): Outcome

  def entail(c: Constraint, i: Int): Outcome

  def entailIfFree(c: Constraint): Outcome

  def entailIfFree(c: Constraint, doms: Array[Domain]): Outcome

  def entailIf(c: Constraint, f: ProblemState => Boolean): Outcome

  def updateDom(v: Variable, d: Domain): Outcome

  def assign(v: Variable, value: Int): Outcome

  def remove(v: Variable, value: Int): Outcome

  def doms(vs: Array[Variable]) = vs.map(dom(_))

  def dom(v: Variable): Domain

  def card(v: Variable) = dom(v).size

  def span(v: Variable): Interval = dom(v).span

  //def boolDom(v: Variable): BooleanDomain = dom(v).asInstanceOf[BooleanDomain]

  def updateState[S <: AnyRef](c: StatefulConstraint[S], newState: S): Outcome

  //def domainsOption: Option[IndexedSeq[Domain]]

  //def toString(problem: Problem): String
  def toState: ProblemState

  //def isEntailed(c: Constraint): Boolean
  def activeConstraints(v: Variable): BitVector

  def apply[S <: AnyRef](c: StatefulConstraint[S]): S

  def isState: Boolean

  def fold[A](s: Traversable[A])(f: (ProblemState, A) => Outcome): Outcome = {
    var state = this
    for (e <- s) {
      if (state.isState) {
        state = f(state.toState, e)
      } else {
        return state
      }
    }
    state
  }

  def dueTo(cause: => (Constraint, Seq[Variable])): Outcome
}

object Contradiction {
  def apply(to: Variable): Contradiction = Contradiction(Seq(to))

  def apply(to: Seq[Variable]): Contradiction = Contradiction(None, Seq.empty, to)
}

case class Contradiction(cause: Option[Constraint], from: Seq[Variable], to: Seq[Variable]) extends Outcome {
  def andThen(f: ProblemState => Outcome) = this

  def orElse[A >: ProblemState](f: => A) = f

  def map[A](f: ProblemState => A) = None

  def filterDom(v: Variable)(f: Int => Boolean): Outcome = this

  def shaveDom(v: Variable, lb: Int, ub: Int): Outcome = this

  def entailIfFree(c: Constraint): Outcome = this

  def entailIfFree(c: Constraint, doms: Array[Domain]): Outcome = this

  def entailIf(c: Constraint, f: ProblemState => Boolean): Outcome = this

  def removeTo(v: Variable, ub: Int): Outcome = this

  def removeFrom(v: Variable, lb: Int): Outcome = this

  def removeUntil(v: Variable, ub: Int): Outcome = this

  def removeAfter(v: Variable, lb: Int): Outcome = this

  def updateDom(v: Variable, d: Domain): Outcome = this

  def remove(v: Variable, value: Int): Outcome = this

  def dom(v: Variable): Domain = throw new UNSATException("Tried to get a domain from a Contradiction")

  def toState = throw new UNSATException("Tried to get state from a Contradiction")

  def apply[S <: AnyRef](c: StatefulConstraint[S]): S = throw new UNSATException("Tried to get state from a Contradiction")

  def assign(v: Variable, value: Int): concrete.Outcome = this

  def entail(c: Constraint): concrete.Outcome = this

  def entail(c: Constraint, i: Int): concrete.Outcome = this

  def activeConstraints(v: Variable): BitVector = throw new UNSATException("Tried to get state from a Contradiction")

  def updateState[S <: AnyRef](c: StatefulConstraint[S], newState: S): Outcome = this

  def isState = false

  def dueTo(cause: => (Constraint, Seq[Variable])) = Contradiction(Some(cause._1), this.from ++ cause._2, to)
}

object ProblemState {


  def apply(problem: Problem, decisionVariables: Set[Variable]): Outcome = {
    val doms = scala.collection.immutable.Vector(problem.variables.map(_.initDomain): _*)
    ProblemState(
      doms,
      Vector(),
      EntailmentManagerLight(problem.variables))
      .padConstraints(problem.constraints, problem.maxCId)
  }

  def isFree(doms: Array[Domain]): Boolean = {
    var one = false
    var i = doms.length - 1
    while (i >= 0) {
      if (!doms(i).isAssigned) {
        if (one) {
          return false
        }
        one = true
      }
      i -= 1
    }
    true
  }

  def singleFree(doms: Array[Domain]): Option[Int] = {
    var f = -1
    var i = doms.length - 1
    while (i >= 0) {
      if (!doms(i).isAssigned) {
        if (f >= 0) return None
        f = i
      }
      i -= 1
    }
    if (f < 0) None else Some(f)

  }

}

case class ProblemState(
                         val domains: scala.collection.immutable.Vector[Domain],
                         val constraintStates: Vector[AnyRef],
                         val entailed: EntailmentManagerLight,
                         val data: IdentityMap[Any, Any] = IdentityMap()) extends Outcome
  with LazyLogging {

  def wDeg(v: Variable) = entailed.wDeg(v)

  def isState = true

  def andThen(f: ProblemState => Outcome) = f(this)

  def orElse[A >: ProblemState](f: => A) = this

  def map[A](f: ProblemState => A) = Some(f(this))

  def apply[S <: AnyRef](c: StatefulConstraint[S]): S =
    constraintStates(c.id).asInstanceOf[S]

  def updateState[S <: AnyRef](c: StatefulConstraint[S], newState: S): ProblemState = {
    val id = c.id
    if (constraintStates(id) eq newState) {
      this
    } else {
      new ProblemState(domains, constraintStates.updated(id, newState), entailed, data)
    }
  }

  def padConstraints(constraints: Seq[Constraint], lastId: Int): Outcome = {
    if (constraintStates.length > lastId) {
      require(lastId < 0 || constraintStates.isDefinedAt(lastId),
        s"$constraintStates($lastId) is not defined")

      this
    } else {
      val padded = constraintStates.padTo(lastId + 1, null)

      val newConstraints = constraints.view.filter(_.id >= constraintStates.size)

      //println(newConstraints.toList)

      newConstraints.foldLeft(ProblemState(domains, padded, entailed.addConstraints(newConstraints), data): Outcome) {
        case (out, c) => out.andThen(c.init(_))
      }
      //        .andThen { newState =>
      //          ProblemState(newState.domains, newState.constraintStates, entailed)
      //        }

    }
  }

  //def isEntailed(c: Constraint): Boolean = entailed(c)

  def activeConstraints(v: Variable): BitVector = entailed.active(v)

  def shaveDomNonEmpty(variable: Variable, itv: Interval): ProblemState = {
    updateDomNonEmpty(variable, dom(variable) & itv)
  }

  def assign(v: Variable, value: Int): ProblemState = {
    val assigned = dom(v).assign(value)
    assert(assigned.nonEmpty)
    updateDomNonEmpty(v, dom(v).assign(value))
  }

  //def isEntailed(c: Constraint): Boolean = entailed(c.id)

  def tryAssign(v: Variable, value: Int): Outcome = {
    val d = dom(v)
    if (d.present(value)) {
      if (d.isAssigned) {
        this
      } else {
        updateDomNonEmptyNoCheck(v, d.assign(value))
      }
    } else {
      Contradiction(v)
    }

  }

  def remove(v: Variable, value: Int): Outcome = {
    updateDom(v, dom(v).remove(value))
  }

  def removeIfPresent(v: Variable, value: Int): Outcome = {
    val d = dom(v)
    if (d.present(value)) {
      updateDom(v, d.remove(value))
    } else {
      this
    }
  }

  def filterDom(v: Variable)(f: Int => Boolean) =
    updateDom(v, dom(v).filter(f))

  def shaveDom(v: Variable, lb: Int, ub: Int): Outcome =
    updateDom(v, dom(v) & (lb, ub))

  def removeTo(v: Variable, ub: Int): Outcome =
    updateDom(v, dom(v).removeTo(ub))

  def updateDom(v: Variable, newDomain: Domain): Outcome = {
    if (newDomain.isEmpty) {
      Contradiction(v)
    } else {
      updateDomNonEmpty(v, newDomain)
    }
  }

  def updateDomNonEmpty(variable: Variable, newDomain: Domain): ProblemState = {

    val oldDomain = dom(variable)
    assert(newDomain subsetOf oldDomain, s"replacing $oldDomain with $newDomain: not a subset!")
    if (oldDomain.size == newDomain.size) {
      assert(oldDomain.subsetOf(newDomain), s"replacing $oldDomain with $newDomain: same size but not equal")
      this
    } else {
      assert(newDomain.size < oldDomain.size, s"replacing $oldDomain with $newDomain: domain size seems to have increased")
      assert(!oldDomain.isInstanceOf[BooleanDomain] || newDomain.isInstanceOf[BooleanDomain], s"replaced $oldDomain with $newDomain: type changed")
      //assert(!oldDomain.isInstanceOf[IntDomain] || newDomain.isInstanceOf[IntDomain], s"replaced $oldDomain with $newDomain: type changed")
      updateDomNonEmptyNoCheck(variable, newDomain)
    }
  }

  def updateDomNonEmptyNoCheck(variable: Variable, newDomain: Domain): ProblemState = {
    assert(newDomain.nonEmpty)
    assert(dom(variable) ne newDomain)
    val id = variable.id
    assert(id >= 0 || (dom(variable) eq newDomain), s"$variable updated to $newDomain is not a problem variable")
    new ProblemState(domains.updated(id, newDomain), constraintStates, entailed, data)
  }

  def dom(v: Variable): Domain = {
    val id = v.id
    if (id < 0) v.initDomain else domains(id)
  }

  override def removeFrom(v: Variable, lb: Int): Outcome =
    updateDom(v, dom(v).removeFrom(lb))

  override def removeUntil(v: Variable, ub: Int): Outcome =
    updateDom(v, dom(v).removeUntil(ub))

  override def removeAfter(v: Variable, lb: Int): Outcome =
    updateDom(v, dom(v).removeAfter(lb))

  def entailIfFree(c: Constraint) = c.singleFree(this).map(entail(c, _)).getOrElse(this)

  def entailIfFree(c: Constraint, doms: Array[Domain]) = ProblemState.singleFree(doms).map(entail(c, _)).getOrElse(this)

  def entail(c: Constraint, i: Int): ProblemState = {
    new ProblemState(domains, constraintStates, entailed.entail(c, i), data)
  }

  def entailIf(c: Constraint, f: ProblemState => Boolean) = {
    if (f(this)) entail(c) else this
  }

  def entail(c: Constraint): ProblemState = {
    new ProblemState(domains, constraintStates, entailed.entail(c, this), data)
    //else this
  }

  def toState: ProblemState = this

  def dueTo(cause: => (Constraint, Seq[Variable])) = this

  def updateData(key: Any, value: Any) =
    new ProblemState(domains, constraintStates, entailed, data + ((key, value)))

  def getData[A](key: Any) = data(key).asInstanceOf[A]
}