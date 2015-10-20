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
  def orElse(f: => Outcome): Outcome

  def filterDom(v: Variable)(f: Int => Boolean): Outcome

  def shaveDom(v: Variable, lb: Int, ub: Int): Outcome
  def shaveDom(v: Variable, itv: Interval): Outcome = shaveDom(v, itv.lb, itv.ub)

  def removeTo(v: Variable, ub: Int): Outcome
  def removeFrom(v: Variable, lb: Int): Outcome
  def removeUntil(v: Variable, ub: Int): Outcome
  def removeAfter(v: Variable, lb: Int): Outcome

  def entail(c: Constraint): Outcome

  def entailIfFree(c: Constraint): Outcome

  def entailIf(c: Constraint, f: ProblemState => Boolean): Outcome

  def updateDom(v: Variable, d: Domain): Outcome

  def updateAll(vars: Iterable[Variable])(f: Domain => Domain): Outcome = updateAll(vars.iterator)(f)
  def updateAll(vars: Iterator[Variable])(f: Domain => Domain): Outcome = {
    var ch = this
    while (vars.hasNext) {
      if (ch eq Contradiction) return Contradiction
      var v = vars.next
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

  def dom(v: Variable): Domain

  def card(v: Variable) = dom(v).length

  def span(v: Variable): Interval = dom(v).span

  def boolDom(v: Variable): BooleanDomain = dom(v).asInstanceOf[BooleanDomain]

  def updateState[S <: AnyRef](c: StatefulConstraint[S], newState: S): Outcome

  def domainsOption: Option[IndexedSeq[Domain]]

  def toString(problem: Problem): String
  def toState: ProblemState

  def isEntailed(c: Constraint): Boolean

  def apply[S](c: StatefulConstraint[S]): S
}

case object Contradiction extends Outcome {
  def andThen(f: ProblemState => Outcome) = Contradiction
  def orElse(f: => Outcome) = f
  def filterDom(v: Variable)(f: Int => Boolean): Outcome = Contradiction
  def shaveDom(v: Variable, lb: Int, ub: Int): Outcome = Contradiction
  def entailIfFree(c: Constraint): Outcome = Contradiction
  def entailIf(c: Constraint, f: ProblemState => Boolean): Outcome = Contradiction
  def removeTo(v: Variable, ub: Int): Outcome = Contradiction
  def removeFrom(v: Variable, lb: Int): Outcome = Contradiction
  def removeUntil(v: Variable, ub: Int): Outcome = Contradiction
  def removeAfter(v: Variable, lb: Int): Outcome = Contradiction
  def updateDom(v: Variable, d: Domain): Outcome = Contradiction
  def remove(v: Variable, value: Int): Outcome = Contradiction
  def dom(v: Variable): Domain = throw new UNSATException("Tried to get a domain from a Contradiction")
  def domainsOption(): Option[IndexedSeq[Domain]] = None
  def toString(problem: Problem) = "Contradiction"
  def toState = throw new UNSATException("Tried to get state from a Contradiction")
  def apply[S](c: StatefulConstraint[S]): S = throw new UNSATException("Tried to get state from a Contradiction")
  def assign(v: Variable, value: Int): concrete.Outcome = Contradiction
  def entail(c: Constraint): concrete.Outcome = Contradiction
  def isEntailed(c: Constraint): Boolean = throw new UNSATException("Tried to get state from a Contradiction")
  def updateState[S](c: StatefulConstraint[S], newState: S): concrete.Outcome = Contradiction
}

object ProblemState {
  //  def apply(domains: Seq[Domain], constraintStates: Seq[AnyRef], entailed: Traversable[Int]): ProblemState =
  //    ProblemState(Vector(domains: _*), Vector(constraintStates: _*), BitVector(entailed))

  def apply(problem: Problem): Outcome = {
    ProblemState(Vector(problem.variables.map(_.initDomain): _*), Vector(), BitVector.empty)
      .padConstraints(problem.constraints, problem.maxCId)
  }
}

final case class ProblemState(
  val domains: Vector[Domain],
  val constraintStates: Vector[AnyRef],
  val entailed: BitVector) extends Outcome
    with LazyLogging {

  def andThen(f: ProblemState => Outcome) = f(this)

  def orElse(f: => Outcome) = this

  def apply[S](c: StatefulConstraint[S]): S = constraintStates(c.id).asInstanceOf[S]

  def updateState[S <: AnyRef](c: StatefulConstraint[S], newState: S): ProblemState = {
    val id = c.id
    if (constraintStates(id) eq newState) {
      this
    } else {
      new ProblemState(domains, constraintStates.updated(id, newState), entailed)
    }
  }

  //  def updateConstraints(f: (Int, Any) => Any) = {
  //    var i = constraintStates.length - 1
  //    val builder = constraintStates.genericBuilder[Any]
  //    builder.sizeHint(constraintStates.length)
  //    while (i < constraintStates.length) {
  //      builder += f(i, constraintStates(i))
  //    }
  //    new ProblemState(domains, builder.result, entailed)
  //  }

  def padConstraints(constraints: Seq[Constraint], lastId: Int): Outcome = {
    val padded = constraintStates.padTo(lastId + 1, null)
    var ps = ProblemState(domains, padded, entailed)
    for (c <- constraints.drop(constraintStates.size)) {
      c.init(ps) match {
        case Contradiction => return Contradiction
        case newState: ProblemState =>
          if (ps ne newState) {
            logger.debug(s"Initializing ${c.toString(ps)} -> ${c.toString(newState)}, entailed = ${newState.isEntailed(c)}")
          }

          ps = newState
      }
    }
    ps
  }

  def entail(c: Constraint): ProblemState = {
    val id = c.id
    if (id >= 0)
      new ProblemState(domains, constraintStates, entailed + id)
    else this
  }

  def isEntailed(c: Constraint): Boolean = entailed(c.id)

  def updateDom(v: Variable, newDomain: Domain): Outcome = {
    if (newDomain.isEmpty) {
      Contradiction
    } else {
      updateDomNonEmpty(v, newDomain)
    }
  }

  def updateDomNonEmpty(variable: Variable, newDomain: Domain): ProblemState = {
    assert(newDomain.nonEmpty)
    val oldDomain = dom(variable)
    if (oldDomain eq newDomain) {
      this
    } else {
      val id = variable.id
      assert(id >= 0 || (dom(variable) eq newDomain), s"$variable updated to $newDomain is not a problem variable")
      assert(newDomain subsetOf oldDomain)
      assert(!oldDomain.isInstanceOf[BooleanDomain] || newDomain.isInstanceOf[BooleanDomain])
      assert(!oldDomain.isInstanceOf[IntDomain] || newDomain.isInstanceOf[IntDomain])
      new ProblemState(domains.updated(id, newDomain), constraintStates, entailed)
    }
  }

  def shaveDomNonEmpty(variable: Variable, itv: Interval): ProblemState = {
    updateDomNonEmpty(variable, dom(variable) & itv)
  }

  def dom(v: Variable): Domain = {
    val id = v.id
    if (id < 0) v.initDomain else domains(id)
  }

  def assigned(v: Variable): Boolean = dom(v).isAssigned

  def assign(v: Variable, value: Int): Outcome = {
    updateDom(v, dom(v).assign(value))
  }

  def remove(v: Variable, value: Int): Outcome = {
    updateDom(v, dom(v).remove(value))
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

  def entailIfFree(c: Constraint) = entailIf(c, c.isFree)

  def entailIf(c: Constraint, f: ProblemState => Boolean) = {
    if (f(this)) entail(c) else this
  }

  def domainsOption = Some(domains)

  def toString(problem: Problem) = problem.toString(this)
  def toState: ProblemState = this

}