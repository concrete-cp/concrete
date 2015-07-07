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
import cspom.util.BitVector

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

abstract class Element(val result: Variable,
                       val index: Variable,
                       val vars: Array[Variable])
  extends Constraint(result +: index +: vars.filter(_ ne null)) {

  protected val varsOption = vars.map(Option.apply)

  lazy val map: Map[Int, Int] = {
    val scopeIndices = scope.zipWithIndex.toMap
    vars.indices.flatMap(i =>
      Option(vars(i)).map(i -> scopeIndices(_))).toMap
  }

  def check(tuple: Array[Int]): Boolean = {
    map.get(tuple(1)).forall(i => tuple(0) == tuple(i))
    //tuple(1) < arity - 2 && tuple(0) == tuple(map(tuple(1)))
  }

  def toString(ps: ProblemState, consistency: String): String = {
    s"${result.toString(ps)} =$consistency= (${index.toString(ps)})th of ${
      vars.toSeq.map {
        case null => "{}"
        case v    => v.toString(ps)
      }
    }"
  }
}

class ElementBC(result: Variable, index: Variable, vars: Array[Variable])
  extends Element(result, index, vars)
  with BC {

  def advise(ps: ProblemState, pos: Int): Int = arity

  def shave(ps: ProblemState) = {
    var ch = List[Int]()

    val resultSpan = ps.span(result)
    /**
     * Revise indices
     */
    ps.filterDom(index) {
      i: Int =>

        i >= 0 && i < vars.length && varsOption(i).forall { v =>
          resultSpan intersects ps.span(v)
        }

    }
      .andThen { ps =>
        /**
         * Revise result
         */
        val union = ps.dom(index).iterator.map(v => ps.span(vars(v))).reduce(_ span _)
        ps.shaveDom(result, union)
      }
      .andThen { ps =>
        /**
         * Revise vars
         */
        val index = ps.dom(this.index)
        if (index.size == 1) {
          ps.shaveDom(vars(index.singleValue), ps.span(this.result))
        } else {
          ps
        }
      }

  }
  def simpleEvaluation: Int = 2
  override def toString(ps: ProblemState) = toString(ps, "BC")
}

class ElementAC(
  result: Variable,
  index: Variable,
  vars: Array[Variable])
  extends Element(result, index, vars) with Removals with BCCompanion {

  val offset = vars.filter(_ ne null).map(_.initDomain.head).min

  def getEvaluation(ps: ProblemState): Int = {
    val scopeSize = this.scopeSize(ps)
    if (skip(ps, scopeSize)) -1 else scopeSize
  }

  def skipIntervals = false

  def revise(ps: ProblemState, modified: List[Int]): Outcome = {
    val resultDom = ps.dom(result)
    /**
     * Revise indices
     */
    ps.filterDom(this.index) { i =>
      i >= 0 && i < vars.length && varsOption(i).forall { v =>
        val d = ps.dom(v)
        resultDom.exists(d.present)
      }

    }
      .andThen { ps =>
        /**
         * Revise result
         */
        var union = BitVector.empty
        for (i <- ps.dom(index)) {
          union |= ps.dom(vars(i)).bitVector(offset)
        }

        ps.filterDom(result)(v => union(v - offset))
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

  override def toString(ps: ProblemState) = toString(ps, "AC")
}