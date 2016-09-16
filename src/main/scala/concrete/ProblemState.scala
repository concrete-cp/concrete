package concrete

import com.typesafe.scalalogging.LazyLogging

import concrete.constraint.Constraint
import concrete.constraint.StatefulConstraint
import cspom.util.BitVector
import concrete.util.Interval
import concrete.util.Vector
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

  def entailIfFree(c: Constraint): Outcome
  def entailIfFree(c: Constraint, doms: Array[Domain]): Outcome

  def entailIf(c: Constraint, f: ProblemState => Boolean): Outcome

  def updateDom(v: Variable, d: Domain): Outcome

  def updateAll(vars: Iterable[Variable])(f: Domain => Domain): Outcome = updateAll(vars.iterator)(f)
  def updateAll(vars: Iterator[Variable])(f: Domain => Domain): Outcome = {
    var ch = this
    while (vars.hasNext) {
      if (ch eq Contradiction) return Contradiction
      val v = vars.next
      ch = ch.updateDom(v, f(dom(v)))
    }
    ch
  }

  def updateDomains(v: Array[Variable], newDomains: IndexedSeq[Domain]): Outcome = {
    var i = v.length - 1
    var ps = this
    while (i >= 0) {
      ps = ps.updateDom(v(i), newDomains(i))
      i -= 1
    }
    ps
  }

  def assign(v: Variable, value: Int): Outcome
  def remove(v: Variable, value: Int): Outcome

  def doms(vs: Array[Variable]) = vs.map(dom(_))

  def dom(v: Variable): Domain

  def card(v: Variable) = dom(v).size

  def span(v: Variable): Interval = dom(v).span

  def boolDom(v: Variable): BooleanDomain = dom(v).asInstanceOf[BooleanDomain]

  def updateState[S <: AnyRef](c: StatefulConstraint[S], newState: S): Outcome

  def domainsOption: Option[IndexedSeq[Domain]]

  def toString(problem: Problem): String
  def toState: ProblemState

  def isEntailed(c: Constraint): Boolean
  def activeConstraints(v: Variable): BitVector

  def apply[S <: AnyRef](c: StatefulConstraint[S]): S

  def isState = this ne Contradiction

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

}

case object Contradiction extends Outcome {
  def andThen(f: ProblemState => Outcome) = Contradiction
  def orElse[A >: ProblemState](f: => A) = f
  def map[A](f: ProblemState => A) = None
  def filterDom(v: Variable)(f: Int => Boolean): Outcome = Contradiction
  def shaveDom(v: Variable, lb: Int, ub: Int): Outcome = Contradiction
  def entailIfFree(c: Constraint): Outcome = Contradiction
  def entailIfFree(c: Constraint, doms: Array[Domain]): Outcome = Contradiction
  def entailIf(c: Constraint, f: ProblemState => Boolean): Outcome = Contradiction
  def removeTo(v: Variable, ub: Int): Outcome = Contradiction
  def removeFrom(v: Variable, lb: Int): Outcome = Contradiction
  def removeUntil(v: Variable, ub: Int): Outcome = Contradiction
  def removeAfter(v: Variable, lb: Int): Outcome = Contradiction
  def updateDom(v: Variable, d: Domain): Outcome = Contradiction
  def remove(v: Variable, value: Int): Outcome = Contradiction
  def dom(v: Variable): Domain = throw new UNSATException("Tried to get a domain from a Contradiction")
  def domainsOption: Option[IndexedSeq[Domain]] = None
  def toString(problem: Problem) = "Contradiction"
  def toState = throw new UNSATException("Tried to get state from a Contradiction")
  def apply[S <: AnyRef](c: StatefulConstraint[S]): S = throw new UNSATException("Tried to get state from a Contradiction")
  def assign(v: Variable, value: Int): concrete.Outcome = Contradiction
  def entail(c: Constraint): concrete.Outcome = Contradiction
  def isEntailed(c: Constraint): Boolean = throw new UNSATException("Tried to get state from a Contradiction")
  def activeConstraints(v: Variable): BitVector = throw new UNSATException("Tried to get state from a Contradiction")
  def updateState[S <: AnyRef](c: StatefulConstraint[S], newState: S): concrete.Outcome = Contradiction
}

object ProblemState {
  //  def apply(domains: Seq[Domain], constraintStates: Seq[AnyRef], entailed: Traversable[Int]): ProblemState =
  //    ProblemState(Vector(domains: _*), Vector(constraintStates: _*), BitVector(entailed))

  def apply(problem: Problem): Outcome = {
    ProblemState(
      Vector(problem.variables.map(_.initDomain): _*),
      Vector(),
      new EntailmentManager(problem.variables))
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
}

class EntailmentManager(
    val entailed: BitVector,
    var activeConstraints: Vector[BitVector],
    var modified: BitVector) {

  def this(variables: Seq[Variable]) =
    this(
      BitVector.empty,
      Vector(variables.map(v => BitVector.filled(v.constraints.length)): _*),
      BitVector.empty)

  def addConstraints(constraints: Seq[Constraint]): EntailmentManager = {
    var ac = activeConstraints
    for (c <- constraints) {
      require(!entailed(c.id))
      for (i <- c.scope.indices) {
        val vid = c.scope(i).id
        /* Fake variables (constants) may appear in constraints */
        if (vid >= 0) {
          ac = ac.updated(vid, ac(vid) + c.positionInVariable(i))
        }
      }
    }
    new EntailmentManager(entailed, ac, modified)
  }

  def apply(c: Constraint): Boolean = entailed(c.id)

  def entail(c: Constraint): EntailmentManager = {
    var mod = modified

    var i = c.arity - 1
    while (i >= 0) {

      val vid = c.scope(i).id
      /* Fake variables (constants) may appear in constraints */
      if (vid >= 0) {
        mod += vid
      }
      i -= 1
    }
    new EntailmentManager(entailed + c.id, activeConstraints, mod)

  }

  def active(v: Variable): BitVector = {
    val vid = v.id
    val active = activeConstraints(vid)
    if (modified(vid)) {
      val filtered = active.filter(p => !entailed(v.constraints(p).id))
      activeConstraints = activeConstraints.updated(vid, filtered)
      modified -= vid
      filtered
    } else {
      active
    }
  }

  def delazy(variables: Array[Variable]) = {
    for (vid <- modified) {
      val v = variables(vid)
      val filtered = activeConstraints(vid).filter(p => !entailed(v.constraints(p).id))
      activeConstraints = activeConstraints.updated(vid, filtered)
    }
    modified = BitVector.empty
  }
}

case class ProblemState(
  val domains: Vector[Domain],
  val constraintStates: Vector[AnyRef],
  val entailed: EntailmentManager) extends Outcome
    with LazyLogging {

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
      new ProblemState(domains, constraintStates.updated(id, newState), entailed)
    }
  }


  def padConstraints(constraints: Seq[Constraint], lastId: Int): Outcome = {
    if (constraintStates.length > lastId) {
      require(lastId < 0 || constraintStates.isDefinedAt(lastId),
        s"$constraintStates($lastId) is not defined")

      this
    } else {
      val padded = constraintStates.padTo(lastId + 1, null)

      val newConstraints = constraints.view.drop(constraintStates.size)

      newConstraints.foldLeft(ProblemState(domains, padded, entailed): Outcome) {
        case (out, c) => out.andThen(c.init)
      }
        .andThen { newState =>
          ProblemState(newState.domains, newState.constraintStates, entailed.addConstraints(newConstraints))
        }
 
    }
  }

  def isEntailed(c: Constraint): Boolean = entailed(c)

  def entail(c: Constraint): ProblemState = {
    new ProblemState(domains, constraintStates, entailed.entail(c))
    //else this
  }

  def activeConstraints(v: Variable): BitVector = entailed.active(v)

  //def isEntailed(c: Constraint): Boolean = entailed(c.id)

  def updateDom(v: Variable, newDomain: Domain): Outcome = {
    if (newDomain.isEmpty) {
      Contradiction
    } else {
      updateDomNonEmpty(v, newDomain)
    }
  }

  def updateDomNonEmpty(variable: Variable, newDomain: Domain): ProblemState = {

    val oldDomain = dom(variable)
    if (oldDomain eq newDomain) {
      this
    } else {
      assert(newDomain.size < oldDomain.size, s"replacing $oldDomain with $newDomain, same size but different instance")
      assert(newDomain subsetOf oldDomain)
      assert(!oldDomain.isInstanceOf[BooleanDomain] || newDomain.isInstanceOf[BooleanDomain])
      assert(!oldDomain.isInstanceOf[IntDomain] || newDomain.isInstanceOf[IntDomain])
      updateDomNonEmptyNoCheck(variable, newDomain)
    }
  }

  def updateDomNonEmptyNoCheck(variable: Variable, newDomain: Domain): ProblemState = {
    require(newDomain.nonEmpty)
    assert(dom(variable) ne newDomain)
    val id = variable.id
    assert(id >= 0 || (dom(variable) eq newDomain), s"$variable updated to $newDomain is not a problem variable")
    new ProblemState(domains.updated(id, newDomain), constraintStates, entailed)
  }

  def shaveDomNonEmpty(variable: Variable, itv: Interval): ProblemState = {
    updateDomNonEmpty(variable, dom(variable) & itv)
  }

  def dom(v: Variable): Domain = {
    val id = v.id
    if (id < 0) v.initDomain else domains(id)
  }

  def assigned(v: Variable): Boolean = dom(v).isAssigned

  def assign(v: Variable, value: Int): ProblemState = {
    val assigned = dom(v).assign(value)
    assert(assigned.nonEmpty)
    updateDomNonEmpty(v, dom(v).assign(value))
  }

  def tryAssign(v: Variable, value: Int): Outcome = {
    val d = dom(v)
    if (d.present(value)) {
      updateDomNonEmpty(v, d.assign(value))
    } else {
      Contradiction
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

  override def removeFrom(v: Variable, lb: Int): Outcome =
    updateDom(v, dom(v).removeFrom(lb))

  override def removeUntil(v: Variable, ub: Int): Outcome =
    updateDom(v, dom(v).removeUntil(ub))

  override def removeAfter(v: Variable, lb: Int): Outcome =
    updateDom(v, dom(v).removeAfter(lb))

  def entailIfFree(c: Constraint) = if (c.isFree(this)) entail(c) else this

  def entailIfFree(c: Constraint, doms: Array[Domain]) = if (ProblemState.isFree(doms)) entail(c) else this

  def entailIf(c: Constraint, f: ProblemState => Boolean) = {
    if (f(this)) entail(c) else this
  }

  def domainsOption = Some(domains)

  def delazy(vars: Array[Variable]) = entailed.delazy(vars)

  def toString(problem: Problem) = problem.toString(this)
  def toState: ProblemState = this

}