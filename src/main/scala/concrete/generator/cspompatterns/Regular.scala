package concrete.generator.cspompatterns

import cspom.CSPOM.SeqOperations
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}
import cspom.extension.MDDRelation
import cspom.variable.{CSPOMConstant, CSPOMSeq, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}
import mdd.{MDD, MDD0, MDDLeaf}
import ConstraintCompiler._

import scala.collection.mutable

object Regular extends ConstraintCompilerNoData {

  def functions = Functions("regular")

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = {
    constraint.result.isTrue
  }

  /**
    * The sequence of values in array "x"' (which must all be in the range 1..S)
    * is accepted by the DFA of "Q"' states with input 1..S and transition
    * function "d"' (which maps (1..Q, 1..S) -> 0..Q)) and initial state "q0"'
    * (which must be in 1..Q) and accepting states "F"' (which all must be in
    * 1..Q).  We reserve state 0 to be an always failing state.
    */

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val Seq(
    CSPOMSeq(x: Seq[SimpleExpression[Int]]@unchecked),
    CSPOMConstant(q0),
    CSPOMConstant.seq(f: Seq[_])) = constraint.arguments

    val Some(dfa) = constraint.getParam[Map[(Any, Any), Any]]("dfa")

    val values = dfa.keys.map(_._2).toSeq.distinct.toIndexedSeq

    val regular = mdd(IndexedSeq.fill(x.length)(values), q0, f.toSet, dfa).reduce()
    //
    replaceCtr(constraint, x in new MDDRelation(regular), problem)
  }

  def mdd[T](v: IndexedSeq[Seq[T]], initState: T, finalStates: Set[T], dfa: Map[(T, T), T]): MDD = {
    val cache = new mutable.HashMap[(Int, T), MDD]()

    def parse(depth: Int, state: T): MDD = cache.getOrElseUpdate((depth, state), {
      if (depth >= v.length) {
        if (finalStates(state)) {
          MDDLeaf
        } else {
          MDD0
        }
      } else {

        val trie = for {value <- v(depth)
                        nextState <- dfa.get((state, value))
                        subMDD: MDD = parse(depth + 1, nextState)
                        if subMDD.nonEmpty} yield {
          concrete.util.Math.any2Int(value) -> subMDD
        }

        MDD.fromTrie(trie)
      }
    })

    parse(0, initState)

  }

  def selfPropagation = false

}
