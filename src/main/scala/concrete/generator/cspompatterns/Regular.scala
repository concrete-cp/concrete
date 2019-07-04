package concrete.generator.cspompatterns

import concrete.util.Math.any2Int
import cspom.CSPOM.SeqOperations
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.{ConstraintCompilerNoData, Delta, Functions}
import cspom.extension.MDDRelation
import cspom.variable.{CSPOMConstant, CSPOMSeq, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}
import mdd.{MDD, MDD0, MDDLeaf}

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

    val dfaAny = constraint.getParam[Map[(Any, Any), Any]]("dfa").get

    val dfa: Map[(Int, Int), Int] = dfaAny.iterator
      .map {
        case ((source, value), dest) => (any2Int(source), any2Int(value)) -> any2Int(dest)
      }
      .toMap

    val values = dfa.keys.map(_._2).toSeq.distinct.toIndexedSeq

    val regular = mdd(
      IndexedSeq.fill(x.length)(values),
      any2Int(q0),
      f.map(any2Int).toSet,
      dfa).reduce()

    logger.info(s"MDD $regular was generated for constraint ${constraint.toString(problem.displayName)}")

    if (regular.isEmpty) {
      logger.warn(s"MDD $regular is empty for constraint ${constraint.toString(problem.displayName)}")
    }

    //
    replaceCtr(constraint, x in new MDDRelation(regular, reduced = true), problem)
  }

  def mdd(v: IndexedSeq[Seq[Int]], initState: Int, finalStates: Set[Int], dfa: Map[(Int, Int), Int]): MDD = {
    val cache = new mutable.HashMap[(Int, Int), MDD]()

    def parse(depth: Int, state: Int): MDD = cache.getOrElseUpdate((depth, state), {
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
