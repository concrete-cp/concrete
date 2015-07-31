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
  def filterDom(id: Int)(f: Int => Boolean): Outcome
  def filterDom(v: Variable)(f: Int => Boolean): Outcome

  def shaveDom(id: Int, lb: Int, ub: Int): Outcome
  def shaveDom(id: Int, itv: Interval): Outcome = shaveDom(id, itv.lb, itv.ub)

  def shaveDom(v: Variable, lb: Int, ub: Int): Outcome
  def shaveDom(v: Variable, itv: Interval): Outcome = shaveDom(v, itv.lb, itv.ub)

  def removeTo(v: Variable, ub: Int): Outcome
  def removeFrom(v: Variable, lb: Int): Outcome
  def removeUntil(v: Variable, ub: Int): Outcome
  def removeAfter(v: Variable, lb: Int): Outcome

  def removeTo(id: Int, ub: Int): Outcome
  def removeFrom(id: Int, lb: Int): Outcome
  def removeUntil(id: Int, ub: Int): Outcome
  def removeAfter(id: Int, lb: Int): Outcome

  def entail(id: Int): Outcome
  def entail(c: Constraint): Outcome = entail(c.id)

  def entailIfFree(c: Constraint): Outcome = entailIf(c, c.isFree)

  def entailIf(c: Constraint, f: ProblemState => Boolean): Outcome

  def updateDom(v: Variable, d: Domain): Outcome
  def updateDom(id: Int, d: Domain): Outcome

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

  def assign(p: Pair): Outcome = assign(p.variable, p.value)
  def remove(p: Pair): Outcome = remove(p.variable, p.value)

  def assign(v: Variable, value: Int): Outcome = assign(v.id, value)
  def remove(v: Variable, value: Int): Outcome
  //  = try remove(v.id, value)
  //  catch {
  //    case e: IndexOutOfBoundsException =>
  //      throw new IllegalArgumentException(s"Variable ${v.toString(toState)} is not a problem variable, trying to remove $value", e)
  //  }

  def assign(id: Int, value: Int): Outcome
  def remove(id: Int, value: Int): Outcome

  def dom(id: Int): Domain
  def dom(v: Variable): Domain

  def card(id: Int) = dom(id).length
  def card(v: Variable) = dom(v).length

  def span(id: Int): Interval = dom(id).span
  def span(v: Variable): Interval = dom(v).span

  def boolDom(id: Int): BooleanDomain
  def boolDom(v: Variable): BooleanDomain = boolDom(v.id)

  def updateState(id: Int, newState: AnyRef): Outcome

  def updateState[S <: AnyRef](c: StatefulConstraint[S], newState: S): Outcome =
    updateState(c.id, newState)

  def domainsOption: Option[IndexedSeq[Domain]]

  def toString(problem: Problem): String
  def toState: ProblemState

  def isEntailed(id: Int): Boolean
  def isEntailed(c: Constraint): Boolean = isEntailed(c.id)
}

case object Contradiction extends Outcome {
  def andThen(f: ProblemState => Outcome) = Contradiction
  def filterDom(id: Int)(f: Int => Boolean): Outcome = Contradiction
  def filterDom(v: Variable)(f: Int => Boolean): Outcome = Contradiction
  def shaveDom(id: Int, lb: Int, ub: Int): Outcome = Contradiction
  def shaveDom(v: Variable, lb: Int, ub: Int): Outcome = Contradiction
  def entailIf(c: Constraint, f: ProblemState => Boolean): Outcome = Contradiction
  def entail(id: Int): Outcome = Contradiction
  def removeTo(id: Int, ub: Int): Outcome = Contradiction
  def removeFrom(id: Int, lb: Int): Outcome = Contradiction
  def removeUntil(id: Int, ub: Int): Outcome = Contradiction
  def removeAfter(id: Int, ub: Int): Outcome = Contradiction
  def removeTo(v: Variable, ub: Int): Outcome = Contradiction
  def removeFrom(v: Variable, lb: Int): Outcome = Contradiction
  def removeUntil(v: Variable, ub: Int): Outcome = Contradiction
  def removeAfter(v: Variable, lb: Int): Outcome = Contradiction
  def updateDom(id: Int, d: Domain): Outcome = Contradiction
  def updateDom(v: Variable, d: Domain): Outcome = Contradiction
  def assign(id: Int, value: Int): Outcome = Contradiction
  def remove(v: Variable, value: Int): Outcome = Contradiction
  def remove(id: Int, value: Int): Outcome = Contradiction
  def dom(id: Int): Domain = throw new UNSATException("Tried to get a domain from a Contradiction")
  def dom(v: Variable): Domain = throw new UNSATException("Tried to get a domain from a Contradiction")
  def boolDom(id: Int): BooleanDomain = throw new UNSATException("Tried to get a domain from a Contradiction")

  def updateState(id: Int, newState: AnyRef): Outcome = Contradiction
  def domainsOption(): Option[IndexedSeq[Domain]] = None
  def toString(problem: Problem) = "Contradiction"
  def toState = throw new UNSATException("Tried to get state from a Contradiction")
  def isEntailed(id: Int) = throw new UNSATException("Tried to get entailement info from a Contradiction")
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

  def apply[S <: AnyRef](c: StatefulConstraint[S]): S = constraintStates(c.id).asInstanceOf[S]

  def updateState(id: Int, newState: AnyRef): ProblemState = {
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

  def entail(id: Int): ProblemState = {
    //logger.warn(s"constraint $id is already entailed")
    new ProblemState(domains, constraintStates, entailed + id)
  }

  def isEntailed(id: Int): Boolean = entailed(id)

  def updateDom(id: Int, newDomain: Domain): Outcome =
    if (newDomain.isEmpty) {
      Contradiction
    } else {
      updateDomNonEmpty(id, newDomain)
    }

  def updateDom(v: Variable, newDomain: Domain): Outcome = {
    if (newDomain.isEmpty) {
      Contradiction
    } else {
      updateDomNonEmpty(v, newDomain)
    }
  }

  private def updateDomNonEmpty(id: Int, oldDomain: Domain, newDomain: Domain): ProblemState = {
    assert(newDomain.nonEmpty)
    if (oldDomain eq newDomain) {
      this
    } else {
      assert(newDomain subsetOf oldDomain)
      assert(!oldDomain.isInstanceOf[BooleanDomain] || newDomain.isInstanceOf[BooleanDomain])
      assert(!oldDomain.isInstanceOf[IntDomain] || newDomain.isInstanceOf[IntDomain])
      new ProblemState(domains.updated(id, newDomain), constraintStates, entailed)
    }
  }

  def updateDomNonEmpty(id: Int, newDomain: Domain): ProblemState = {
    updateDomNonEmpty(id, dom(id), newDomain)
  }

  def updateDomNonEmpty(variable: Variable, newDomain: Domain): ProblemState = {
    assert(variable.id >= 0 || (dom(variable) eq newDomain), s"$variable updated to $newDomain is not a problem variable")
    updateDomNonEmpty(variable.id, dom(variable), newDomain)
  }

  def shaveDomNonEmpty(variable: Variable, itv: Interval): ProblemState = {
    updateDomNonEmpty(variable, dom(variable) & itv)
  }

  def dom(id: Int): Domain = domains(id)
  def dom(v: Variable): Domain = {
    val id = v.id
    if (id < 0) v.initDomain else domains(id)
  }

  def boolDom(id: Int): BooleanDomain = domains(id).asInstanceOf[BooleanDomain]

  def assign(id: Int, value: Int): Outcome = {
    updateDom(id, domains(id).assign(value))
  }

  def remove(v: Variable, value: Int): Outcome = {
    updateDom(v, dom(v).remove(value))
  }

  def remove(id: Int, value: Int): Outcome = {
    updateDom(id, domains(id).remove(value))
  }

  def filterDom(id: Int)(f: Int => Boolean) =
    updateDom(id, domains(id).filter(f))

  def filterDom(v: Variable)(f: Int => Boolean) =
    updateDom(v, dom(v).filter(f))

  def shaveDom(id: Int, lb: Int, ub: Int): Outcome =
    updateDom(id, domains(id) & (lb, ub))

  def shaveDom(v: Variable, lb: Int, ub: Int): Outcome =
    updateDom(v, dom(v) & (lb, ub))

  def removeTo(id: Int, ub: Int): Outcome =
    updateDom(id, domains(id).removeTo(ub))

  def removeTo(v: Variable, ub: Int): Outcome =
    updateDom(v, dom(v).removeTo(ub))

  def removeFrom(id: Int, lb: Int): Outcome =
    updateDom(id, domains(id).removeFrom(lb))

  override def removeFrom(v: Variable, lb: Int): Outcome =
    updateDom(v, dom(v).removeFrom(lb))

  def removeUntil(id: Int, ub: Int): Outcome =
    updateDom(id, domains(id).removeUntil(ub))

  override def removeUntil(v: Variable, ub: Int): Outcome =
    updateDom(v, dom(v).removeUntil(ub))

  def removeAfter(id: Int, lb: Int): Outcome =
    updateDom(id, domains(id).removeAfter(lb))

  override def removeAfter(v: Variable, lb: Int): Outcome =
    updateDom(v, dom(v).removeAfter(lb))

  def entailIf(c: Constraint, f: ProblemState => Boolean) = {
    if (f(this)) entail(c.id) else this
  }

  def domainsOption = Some(domains)

  def toString(problem: Problem) = problem.toString(this)
  def toState: ProblemState = this

}