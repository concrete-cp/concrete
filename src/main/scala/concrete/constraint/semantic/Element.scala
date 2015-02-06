package concrete.constraint.semantic

import scala.collection.mutable.ArrayBuffer
import concrete.Domain
import concrete.Variable
import concrete.constraint.BCCompanion
import concrete.constraint.Constraint
import concrete.constraint.Removals
import concrete.constraint.BC
import concrete.ProblemState
import concrete.Outcome

object Element {
  def apply(result: Variable, index: Variable, varsIdx: Seq[(Int, Variable)]) = {
    //val scope = ArrayBuffer(result, index)

    val lastIndex = varsIdx.map(_._1).max
    val vars = Array.ofDim[Variable](lastIndex + 1)

    //    val scopeIndex = Array.fill(lastIndex + 1)(-1)
    for ((i, v) <- varsIdx) {
      vars(i) = v
      //      scopeIndex(i) = scope.size
      //scope += v
    }

    Seq(
      new ElementBC(result, index, vars),
      new ElementAC(result, index, vars))
  }
}

class ElementBC(
  val result: Variable,
  val index: Variable,
  val vars: Array[Variable])
  extends Constraint(result +: index +: vars.filter(_ ne null)) with BC {

  def advise(ps: ProblemState, pos: Int): Int = arity

  def check(tuple: Array[Int]): Boolean = {
    tuple(1) < arity - 2 && tuple(0) == tuple(tuple(1) + 2)
  }
  def shave(ps: ProblemState) = {
    var ch = List[Int]()

    val resultSpan = ps.span(result)
    /**
     * Revise indices
     */
    ps.filterDom(index) {
      v: Int =>
        v >= 0 && v < vars.length && (vars(v) ne null) &&
          (resultSpan intersects ps.span(vars(v)))
    }
      .andThen { ps =>
        println(s"Indices ${index.toString(ps)}, revising result")
        /**
         * Revise result
         */
        val union = ps.dom(index).iterator.map(v => ps.span(vars(v))).reduce(_ span _)
        ps.shaveDom(result, union)
      }
      .andThen { ps =>
        println(s"Result ${result.toString(ps)}, revising vars")
        /**
         * Revise vars
         */
        val index = ps.dom(this.index)
        if (index.size == 1) {
          val i = index.singleValue
          ps.shaveDom(scope(i), ps.span(this.result))
        } else {
          ps
        }
      }

  }
  def simpleEvaluation: Int = 2
  override def toString(ps: ProblemState) = {
    s"${result.toString(ps)} =BC= (${index.toString(ps)})th of ${
      vars.toSeq.map {
        case null => "{}"
        case v    => v.toString(ps)
      }
    }"
  }
}

class ElementAC(
  val result: Variable,
  val index: Variable,
  val vars: Array[Variable])
  extends Constraint(result +: index +: vars.filter(_ ne null)) with Removals with BCCompanion {

  def getEvaluation(ps: ProblemState): Int = if (skip(ps)) -1 else scopeSize(ps)

  def check(tuple: Array[Int]): Boolean = {
    tuple(1) < arity - 2 && tuple(0) == tuple(tuple(1) + 2)
  }

  def skipIntervals = false

  def revise(ps: ProblemState, modified: List[Int]): Outcome = {
    val resultDom = ps.dom(result)
    /**
     * Revise indices
     */
    ps.filterDom(this.index) { i =>
      i >= 0 && i < vars.length && (vars(i) ne null) &&
        resultDom.exists(ps.dom(vars(i)).present)
    }
      .andThen { ps =>
        /**
         * Revise result
         */
        val union = ps.dom(index).foldLeft(Set[Int]()) {
          (acc, i) => acc ++ ps.dom(vars(i))
        }
        ps.filterDom(result)(union)
      }
      .andThen { ps =>
        /**
         * Revise vars
         */
        val index = ps.dom(this.index)
        if (index.size == 1) {
          ps.filterDom(vars(index.singleValue))(ps.dom(result).present)
        } else ps
      }

  }
  def simpleEvaluation: Int = 3

  override def toString(ps: ProblemState) = {
    s"${result.toString(ps)} =AC= (${index.toString(ps)})th of ${
      vars.toSeq.map {
        case null => "{}"
        case v    => v.toString(ps)
      }
    }"
  }
}