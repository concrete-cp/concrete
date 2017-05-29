package concrete.generator.cspompatterns

import cspom.CSPOM.SeqOperations
import cspom.compiler.ConstraintCompilerNoData
import cspom.extension.MDDRelation
import cspom.variable.{CSPOMConstant, CSPOMSeq, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}
import mdd.{JavaMap, MDD, MDD0, MDDLeaf}

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
    CSPOMSeq(x: Seq[SimpleExpression[Int]]@unchecked),
    CSPOMConstant(q0: Int),
    CSPOMConstant.seq(f: Seq[Int]@unchecked)) = constraint.arguments

    val Some(dfa) = constraint.getParam[Map[(Int, Int), Int]]("dfa")

    val values = dfa.keys.map(_._2).toSeq.distinct.to[collection.IndexedSeq] //x.map(IntExpression.implicits.iterable).toIndexedSeq

    val regular = mdd(IndexedSeq.fill(x.length)(values), q0, f.toSet, dfa)
    //
    replaceCtr(constraint, x in new MDDRelation(regular), problem)
  }

  def mdd(v: IndexedSeq[Seq[Int]], initState: Int, finalStates: Set[Int], dfa: Map[(Int, Int), Int]): MDD = {
    val cache = new JavaMap[(Int, Int), MDD]()

    def parse(depth: Int, state: Int): MDD = cache.getOrElseUpdate((depth, state), {
      if (depth >= v.length) {
        if (finalStates(state)) {
          MDDLeaf
        } else {
          MDD0
        }
      } else {
        MDD(v(depth)
          .flatMap { value => dfa.get((state, value)).map(value -> _) }
          .map {
            case (value, nextState) =>
              value -> parse(depth + 1, nextState)
          }
          .filter(_._2.nonEmpty))
      }
    })

    parse(0, initState)

  }

  def selfPropagation = false

}
