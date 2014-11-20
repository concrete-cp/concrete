package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import scala.collection.mutable.ArrayBuffer
import concrete.constraint.BCCompanion
import concrete.constraint.StatelessBC
import concrete.constraint.Stateless
import concrete.constraint.Removals
import concrete.Revised
import concrete.ReviseOutcome
import concrete.Domain

object Element {
  def apply(result: Variable, index: Variable, varsIdx: Seq[(Int, Variable)]) = {
    val scope = ArrayBuffer(result, index)

    val lastIndex = varsIdx.map(_._1).max
    val vars = Array.ofDim[Variable](lastIndex + 1)
    val scopeIndex = Array.fill(lastIndex + 1)(-1)
    for ((i, v) <- varsIdx) {
      vars(i) = v
      scopeIndex(i) = scope.size
      scope += v
    }

    Seq(
      new ElementBC(result, index, vars, scopeIndex, scope.toArray),
      new ElementAC(result, index, vars, scopeIndex, scope.toArray))
  }
}

class ElementBC(
  val result: Variable,
  val index: Variable,
  val vars: Array[Variable],
  val scopeIndex: Array[Int],
  scope: Array[Variable])
  extends Constraint(scope) with StatelessBC {

  def advise(domains: IndexedSeq[Domain], pos: Int): Int = arity

  def check(tuple: Array[Int]): Boolean = {
    tuple(1) < arity - 2 && tuple(0) == tuple(tuple(1) + 2)
  }
  def shave(domains: IndexedSeq[Domain]) = {
    var ch = List[Int]()
    /**
     * Revise indices
     */
    val index = domains(1).filter {
      v =>
        v >= 0 && v < vars.length && (vars(v) ne null) &&
          (domains(0).span intersects domains(scopeIndex(v)).span)
    }

    /**
     * Revise result
     */
    val union = index.map(v => domains(scopeIndex(v)).span).reduce(_ span _)
    val result = domains(0) & union

    /**
     * Revise vars
     */
    val filtered = if (index.size == 1) {
      val i = index.head
      domains.updated(scopeIndex(i), domains(scopeIndex(i)) & result.span)
    } else domains
    Revised(result +: index +: filtered.drop(2))
  }
  def simpleEvaluation: Int = 2
}

class ElementAC(
  val result: Variable,
  val index: Variable,
  val vars: Array[Variable],
  val scopeIndex: Array[Int],
  scope: Array[Variable])
  extends Constraint(scope) with Removals with BCCompanion {

  type State = Unit

  def getEvaluation(domains: IndexedSeq[Domain]): Int = if (skip(domains)) -1 else scopeSize(domains)

  def initState = Unit

  def check(tuple: Array[Int]): Boolean = {
    tuple(1) < arity - 2 && tuple(0) == tuple(tuple(1) + 2)
  }

  def skipIntervals = false

  def revise(domains: IndexedSeq[Domain], modified: List[Int], s: Unit): ReviseOutcome[Unit] = {
    /**
     * Revise indices
     */
    val index = domains(1).filter {
      i =>
        i >= 0 && i < vars.length && (vars(i) ne null) &&
          domains(0).exists(domains(scopeIndex(i)).present)
    }

    /**
     * Revise result
     */
    val union = index.foldLeft(Set[Int]()) {
      (acc, i) => acc ++ domains(scopeIndex(i))
    }
    val result = domains(0).filter(union)

    /**
     * Revise vars
     */
    val filtered = if (index.size == 1) {
      val i = scopeIndex(index.head)
      domains.updated(i, domains(i).filter(result.present))
    } else domains
    Revised(result +: index +: filtered.drop(2))
  }
  def simpleEvaluation: Int = 3
}