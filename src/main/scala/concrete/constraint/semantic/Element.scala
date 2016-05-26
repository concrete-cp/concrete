package concrete.constraint.semantic

import concrete.Domain
import concrete.Outcome
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Removals
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
      new Element(result, index, vars))
  }
}

class Element(val result: Variable,
              val index: Variable,
              val vars: Array[Variable])
    extends Constraint(result +: index +: vars.filter(_ ne null)) with Removals {

  protected val varsOption = vars.map(Option.apply)

  private var card: Int = _

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

  def getEvaluation(ps: ProblemState): Int = {
    card * ps.card(index)
  }

  override def init(ps: ProblemState) = {
    card = varsOption.flatten.map(ps.dom(_).size).max
    ps.shaveDom(index, 0, vars.length)
  }

  def revise(ps: ProblemState, modified: BitVector): Outcome = {
    val resultDom = ps.dom(result)
    /*
     * Revise indices
     */
    ps.filterDom(this.index) { i =>
      varsOption(i).exists { v => !resultDom.disjoint(ps.dom(v)) }
    }
      .andThen { ps =>
        val index = ps.dom(this.index)

        if (index.isAssigned) {
          val selectedVar = vars(index.singleValue)
          //println(selectedVar.toString(ps))
          val intersect = ps.dom(selectedVar) & resultDom
          ps.updateDom(result, intersect)
            .updateDom(selectedVar, intersect)
        } else {

          /*
           * Revise result
           */
          var union: Domain = null
          for (i <- index) {
            if (null == union) union = ps.dom(vars(i))
            else union |= ps.dom(vars(i))
          }

          ps.updateDom(result, resultDom & union)

        }
      }
  }
  def simpleEvaluation: Int = 3

  override def toString(ps: ProblemState) = toString(ps, "AC")
}