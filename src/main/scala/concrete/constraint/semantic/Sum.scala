package concrete.constraint.semantic

import com.typesafe.scalalogging.LazyLogging
import concrete.Contradiction
import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Singleton
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.StatefulConstraint
import concrete.constraint.TupleEnumerator
import concrete.util.Interval
import concrete.util.Math
import cspom.util.BitVector

import concrete.ParameterManager

object SumMode extends Enumeration {
  type SumMode = Value
  val SumLE = Value("le")
  val SumLT = Value("lt")
  val SumEQ = Value("eq")
  val SumNE = Value("ne")
}

import SumMode._

object Linear {
  def apply(constant: Int, factors: Array[Int], scope: Array[Variable], mode: SumMode.SumMode, pm: ParameterManager) = mode match {
    case SumLE => LinearLe(constant, factors, scope, false, pm)
    case SumLT => LinearLe(constant, factors, scope, true, pm)
    case SumEQ => new LinearEq(constant, factors, scope)
    case SumNE => new LinearNe(constant, factors, scope)
  }

}

abstract class Sum(
    val constant: Int,
    val factors: Array[Int],
    scope: Array[Variable],
    val mode: SumMode.SumMode) extends Constraint(scope) {

  require(factors.forall(_ != 0), this)
  require(factors.size == scope.size)

  private val checkSum: Function[Int, Boolean] =
    mode match {
      case SumLE => 0 <= _
      case SumLT => 0 < _
      case SumEQ => 0 == _
      case SumNE => 0 != _
    }

  def check(t: Array[Int]): Boolean = {
    var i = arity - 1
    var sum = 0
    while (i >= 0) {
      sum += t(i) * factors(i)
      i -= 1
    }
    checkSum(constant - sum)
  }

  def toString(operator: String) = {
    (scope, factors).zipped.map((v, f) => f + "." + v).mkString(" + ") + s" $operator $constant"
  }

  def toString(ps: ProblemState, operator: String) = {
    (scope, factors).zipped.map((v, f) => f + "." + v.toString(ps)).mkString(" + ") + s" $operator $constant"
  }
}

final class SumAC(
    constant: Int,
    factors: Array[Int],
    scope: Array[Variable],
    mode: SumMode.SumMode) extends Sum(constant, factors, scope, mode) with Residues with TupleEnumerator {

  override def toString() = toString(s"${mode}AC")

  override def toString(ps: ProblemState) = toString(ps, s"${mode}AC")
}

trait DomCache extends Constraint {
  def arity: Int
  def scope: Array[Variable]

  protected val doms = new Array[Domain](arity)

  protected def updateDoms(ps: ProblemState, variables: BitVector): Unit = {
    var i = variables.nextSetBit(0)
    while (i >= 0) {
      doms(i) = ps.dom(scope(i))
      i = variables.nextSetBit(i + 1)
    }
  }

  protected def updateDoms(ps: ProblemState, variables: Int): Unit = {
    var i = arity - 1
    while (i >= 0) {
      doms(i) = ps.dom(scope(i))
      i -= 1
    }
  }

  protected def filter(changed: BitVector, ps: ProblemState): Outcome = {
    var filtered: ProblemState = ps //.updateState(id, (cons, vars))
    var i = changed.nextSetBit(0)
    while (i >= 0) {
      filtered = filtered.updateDomNonEmpty(scope(i), doms(i))
      i = changed.nextSetBit(i + 1)
    }
    filtered.entailIfFree(this)
  }
}

final class LinearNe(
    constant: Int,
    factors: Array[Int],
    scope: Array[Variable]) extends Sum(constant, factors, scope, SumNE) with StatefulConstraint[(Int, BitVector)] with DomCache with LazyLogging {

  private def totalSpan(constant: Int, variables: BitVector): (BitVector, Int) = {
    var cons = constant
    val newVar = variables.filter { i =>
      val dom = doms(i)
      if (dom.size == 1) {
        cons -= dom.head * factors(i)
        false
      } else {
        true
      }
    }
    (newVar, cons)
  }

  override def isConsistent(ps: ProblemState) = {
    val (oldCons, oldVar) = ps(this)
    updateDoms(ps, oldVar)
    val (newVar, newCons) = totalSpan(oldCons, oldVar)

    if (newVar.isEmpty && newCons == 0) {
      Contradiction
    } else if (oldVar == newVar) {
      ps
    } else {
      ps.updateState(this, (newCons, newVar))
    }
  }

  override def revise(ps: ProblemState): Outcome = {
    val (oldCons, oldVar) = ps(this)
    updateDoms(ps, oldVar)
    val (newVar, newCons) = totalSpan(oldCons, oldVar)

    newVar.cardinality match {
      case 0 if (newCons == 0) => Contradiction
      case 1 =>
        val p = newVar.nextSetBit(0)
        ps.remove(scope(p), newCons / factors(p)).entail(this)
      case _ => ps.updateState(this, (newCons, newVar))
    }
  }

  override def toString() = toString("!=BC")

  override def toString(ps: ProblemState) = toString(ps, "!=BC")

  def advise(ps: ProblemState, p: Int) = arity * 2

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState) = {
    ps.updateState(this, (constant, BitVector.filled(arity)))
  }
}

object LinearEq {
  val initBound = Interval(0, 0)
}

final class LinearEq(
    constant: Int,
    factors: Array[Int],
    scope: Array[Variable]) extends Sum(constant, factors, scope, SumEQ) with StatefulConstraint[(Int, BitVector)] with DomCache with LazyLogging {

  private def totalSpan(constant: Int, variables: BitVector): (Interval, BitVector, Int) = {
    var bounds = LinearEq.initBound
    var cons = constant
    val newVar = variables.filter { i =>
      val dom = doms(i)
      if (dom.size == 1) {
        cons -= dom.head * factors(i)
        false
      } else {
        bounds += dom.span * factors(i)
        true
      }
    }
    (bounds - cons, newVar, cons)
  }

  override def isConsistent(ps: ProblemState) = {
    val (oldCons, oldVar) = ps(this)
    updateDoms(ps, oldVar)
    val (bounds, newVar, newCons) = totalSpan(oldCons, oldVar)

    if (bounds.contains(0)) {
      ps.updateState(this, (newCons, newVar))
    } else {
      Contradiction
    }
  }

  override def revise(ps: ProblemState): Outcome = {
    val (oldCons, oldVar) = ps(this)
    updateDoms(ps, oldVar)
    val (bounds, newVar, newCons) = totalSpan(oldCons, oldVar)

    process(newVar.nextSetBit(0), newVar, newCons, bounds, ps)
  }

  override def toString() = toString("=BC=")

  override def toString(ps: ProblemState) = toString(ps, "=BC=")

  def advise(ps: ProblemState, p: Int) = arity * 2

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState) = {
    ps.updateState(this, (constant, BitVector.filled(arity)))
  }

  @annotation.tailrec
  private def process(
    i: Int,
    vars: BitVector,
    cons: Int,
    bounds: Interval,
    ps: ProblemState,
    mark: Int = -1,
    hasChanged: BitVector = BitVector.empty): Outcome = {
    if (i < 0) {
      if (cons == 0) {
        filter(hasChanged, ps.updateState(id, (cons, vars)))
      } else {
        Contradiction
      }
    } else if (mark == i) {
      filter(hasChanged, ps.updateState(id, (cons, vars)))
    } else {
      val dom = doms(i)

      val f = factors(i)

      val myBounds = dom.span * f

      val thisBounds = Interval(bounds.lb - myBounds.lb, bounds.ub - myBounds.ub)

      val newDom = dom & (thisBounds / -f)

      if (newDom eq dom) {
        process(vars.nextOrLoop(i + 1), vars, cons, bounds, ps, if (mark < 0) i else mark, hasChanged)
      } else if (newDom.isEmpty) {
        Contradiction
      } else {
        doms(i) = newDom
        if (newDom.size == 1) {
          val prod = newDom.head * f
          val newVars = vars - i
          process(newVars.nextOrLoop(i + 1), newVars, cons - prod, thisBounds + prod, ps, -1, hasChanged + i)
        } else {
          process(vars.nextOrLoop(i + 1), vars, cons, thisBounds + newDom.span * f, ps, i, hasChanged + i)
        }

      }

    }
  }

}

object LinearLe {
  def apply(constant: Int, factors: Array[Int], scope: Array[Variable], strict: Boolean, pm: ParameterManager) = {
    val actualConstant = if (strict) constant - 1 else constant

    if (pm.contains("linear.stateless")) {
      new LinearLeStateless(actualConstant, factors, scope)
    } else {
      new LinearLe(actualConstant, factors, scope)
    }

  }
}

final class LinearLe private (
    constant: Int,
    factors: Array[Int],
    scope: Array[Variable]) extends Sum(constant, factors, scope, SumLE) with StatefulConstraint[(Int, BitVector)] with DomCache with LazyLogging {

  private def totalSpan(constant: Int, variables: BitVector): (Int, BitVector, Int) = {
    var bounds = 0
    var cons = constant
    val newVar = variables.filter { i =>
      val dom = doms(i)
      if (dom.size == 1) {
        cons -= dom.head * factors(i)
        false
      } else {
        bounds += minTimes(dom, factors(i))
        true
      }
    }
    (bounds - cons, newVar, cons)
  }

  override def isConsistent(ps: ProblemState) = {
    val (oldCons, oldVar) = ps(this)
    updateDoms(ps, oldVar)
    val (bounds, newVar, newCons) = totalSpan(oldCons, oldVar)

    if (bounds <= 0) {
      ps.updateState(this, (newCons, newVar))
    } else {
      Contradiction
    }
  }

  override def revise(ps: ProblemState): Outcome = {
    val (oldCons, oldVar) = ps(this)
    updateDoms(ps, oldVar)
    val (bounds, newVar, newCons) = totalSpan(oldCons, oldVar)

    process(newVar.nextSetBit(0), newVar, newCons, bounds, ps)
  }

  override def toString() = toString("<=BC")

  override def toString(ps: ProblemState) = toString(ps, "<=BC")

  def advise(ps: ProblemState, p: Int) = arity * 2

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState) = {
    ps.updateState(this, (constant, BitVector.filled(arity)))
  }

  @annotation.tailrec
  private def process(
    i: Int,
    vars: BitVector,
    cons: Int,
    min: Int,
    ps: ProblemState,
    mark: Int = -1,
    hasChanged: BitVector = BitVector.empty): Outcome = {

    if (i < 0) {
      // All variables are instantiated
      assert(doms.forall(_.size == 1))
      if (0 <= cons) {
        filter(hasChanged, ps.updateState(id, (cons, vars)))
      } else {
        Contradiction
      }
    } else if (mark == i) {
      filter(hasChanged, ps.updateState(id, (cons, vars)))
    } else {
      val dom = doms(i)

      val f = factors(i)

      val thisBounds = min - minTimes(dom, f)

      val newDom = if (f < 0) dom.removeUntil(Math.ceilDiv(thisBounds, -f)) else dom.removeAfter(Math.floorDiv(thisBounds, -f))

      if (newDom eq dom) {
        process(vars.nextOrLoop(i + 1), vars, cons, min, ps, if (mark < 0) i else mark, hasChanged)
      } else if (newDom.isEmpty) {
        Contradiction
      } else {
        doms(i) = newDom
        if (newDom.size == 1) {
          val prod = newDom.head * f
          val newVars = vars - i
          process(newVars.nextOrLoop(i + 1), newVars, cons - prod, thisBounds + prod, ps, -1, hasChanged + i)
        } else {
          process(vars.nextOrLoop(i + 1), vars, cons, thisBounds + minTimes(newDom, f), ps, i, hasChanged + i)
        }
      }

    }
  }

  private def minTimes(dom: Domain, f: Int) = {
    if (f >= 0) dom.head * f else dom.last * f
  }

}

final class LinearLeStateless(
    constant: Int,
    factors: Array[Int],
    scope: Array[Variable]) extends Sum(constant, factors, scope, SumLE) with DomCache with LazyLogging {

  private def totalSpan(constant: Int): Int = {
    var bounds = -constant
    var i = arity - 1
    while (i >= 0) {
      bounds += minTimes(doms(i), factors(i))
      i -= 1
    }
    bounds
  }

  override def isConsistent(ps: ProblemState) = {
    updateDoms(ps, arity)
    if (totalSpan(constant) <= 0) {
      ps
    } else {
      Contradiction
    }
  }

  override def revise(ps: ProblemState): Outcome = {
    updateDoms(ps, arity)
    process(0, totalSpan(constant), ps)
  }

  override def toString() = toString("<=BC")

  override def toString(ps: ProblemState) = toString(ps, "<=BC")

  def advise(ps: ProblemState, p: Int) = arity * 2

  def simpleEvaluation: Int = 3

  override def init(ps: ProblemState) = ps

  @annotation.tailrec
  private def process(
    i: Int,
    min: Int,
    ps: ProblemState,
    mark: Int = -1,
    hasChanged: BitVector = BitVector.empty): Outcome = {

    if (mark == i) {
      filter(hasChanged, ps)
    } else {
      val dom = doms(i)

      val f = factors(i)

      val thisBounds = min - minTimes(dom, f)

      val newDom = if (f < 0) dom.removeUntil(Math.ceilDiv(thisBounds, -f)) else dom.removeAfter(Math.floorDiv(thisBounds, -f))

      if (newDom eq dom) {
        process(nextOrLoop(i + 1), min, ps, if (mark < 0) i else mark, hasChanged)
      } else if (newDom.isEmpty) {
        Contradiction
      } else {
        doms(i) = newDom

        process(nextOrLoop(i + 1), thisBounds + minTimes(newDom, f), ps, i, hasChanged + i)

      }

    }
  }

  private def nextOrLoop(i: Int) = {
    if (i >= arity) 0 else i
  }

  private def minTimes(dom: Domain, f: Int) = {
    if (f >= 0) dom.head * f else dom.last * f
  }

}
