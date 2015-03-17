package concrete.generator.cspompatterns

import scala.collection.immutable.Queue
import scala.collection.mutable.HashMap
import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompilerNoData
import cspom.extension.MDD
import cspom.extension.MDDNode
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.IntExpression.implicits.iterable
import cspom.compiler.Delta
import cspom.variable.CSPOMExpression
import cspom.variable.SimpleExpression

final object Regular extends ConstraintCompilerNoData {

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    constraint.function == 'regular && constraint.result.isTrue
  }

  /**
   * The sequence of values in array 'x' (which must all be in the range 1..S)
   * is accepted by the DFA of 'Q' states with input 1..S and transition
   * function 'd' (which maps (1..Q, 1..S) -> 0..Q)) and initial state 'q0'
   * (which must be in 1..Q) and accepting states 'F' (which all must be in
   * 1..Q).  We reserve state 0 to be an always failing state.
   */

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    val Seq(
      CSPOMSeq(x: Seq[SimpleExpression[Int]] @unchecked),
      CSPOMConstant(q: Int),
      CSPOMConstant(s: Int),
      fd: CSPOMSeq[_],
      CSPOMConstant(q0: Int),
      CSPOMConstant(f: Seq[Int] @unchecked)) = constraint.arguments

    val d: IndexedSeq[IndexedSeq[Int]] =
      fd.grouped(s).map { s =>
        s.map {
          case CSPOMConstant(c: Int) => c
        }
          .toIndexedSeq
      }
        .toIndexedSeq

    require(d.size == q, "Invalid transition matrix for constraint " + constraint)
    //println(d)

    val regular = mdd(x.length, s, d, q0, f.toSet, collection.mutable.Map())
    //println(s"generated MDD for $constraint with ${regular.edges} edges")
    replaceCtr(constraint, CSPOM.SeqOperations(x) in regular, problem)
  }

  def mdd(level: Int, s: Int, d: IndexedSeq[IndexedSeq[Int]], q0: Int, f: Set[Int],
          cache: collection.mutable.Map[(Int, Int), MDD[Int]]): MDD[Int] = {
    //println(level)
    if (q0 == 0) {
      MDD.empty
    } else if (level <= 0) {
      if (f.contains(q0)) MDD.leaf else MDD.empty
    } else {
      cache.getOrElseUpdate((q0, level), {
        //        if (f.contains(q0)) {
        //          MDD.leaf
        //        } else {
        val map = (1 to s).map { v =>
          v -> mdd(level - 1, s, d, d(q0 - 1)(v - 1), f, cache)
        }
        MDDNode(map.filter(_._2.nonEmpty).toMap)
      })
    }
  }

  def selfPropagation = false

}
