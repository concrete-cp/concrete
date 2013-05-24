package concrete.generator.constraint;

import scala.collection.mutable.HashMap
import concrete.Problem
import concrete.Variable
import concrete.constraint.extension.MDD
import concrete.constraint.extension.MDD0
import concrete.constraint.extension.MDDLeaf
import concrete.constraint.semantic.AllDifferent2C
import concrete.constraint.semantic.AllDifferentBC
import concrete.constraint.semantic.ZeroSum
import cspom.constraint.CSPOMConstraint
import cspom.constraint.GeneralConstraint
import concrete.constraint.extension.MDDn
import concrete.constraint.extension.ReduceableExt
import scala.annotation.tailrec

final class ZeroSumGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def generateGeneral(constraint: GeneralConstraint) = {
    val solverVariables = constraint.scope map cspom2concrete toArray

    if (solverVariables exists { _.dom.undefined }) {
      false
    } else {
      val params = constraint.predicate.parameters match {
        case Some(p: Seq[Int]) => p.toArray
        case None => Array.fill(solverVariables.length)(1)
        case _ => throw new IllegalArgumentException("Parameters for zero sum must be a sequence of integer values")
      }
      addConstraint(new ZeroSum(params, solverVariables));
      //addConstraint(new ReduceableExt(solverVariables, zeroSum(solverVariables.toList, params.toList)))
      true;
    }
  }

  @tailrec
  def sum(domains: List[Seq[Int]], factors: List[Int], f: Seq[Int] => Int, currentSum: Int = 0): Int = {
    if (domains.isEmpty) {
      currentSum
    } else {
      sum(domains.tail, factors.tail, f, currentSum + f(domains.head) * factors.head)
    }
  }

  def zeroSum(variables: List[Variable], factors: List[Int]): MDD = {
    val domains = variables.map(_.dom.values.toSeq)
    zeroSum(variables, factors, sum(domains, factors, _.min), sum(domains, factors, _.max))
  }

  def zeroSum(variables: List[Variable], factors: List[Int], min: Int, max: Int, currentSum: Int = 0,
    nodes: HashMap[(Int, Int), MDD] = new HashMap()): MDD = {
    if (variables.isEmpty) {
      require(currentSum == min && currentSum == max)
      if (currentSum == 0) {
        MDDLeaf
      } else {
        MDD0
      }
    } else if (min > 0) {
      MDD0
    } else if (max < 0) {
      MDD0
    } else {
      nodes.getOrElseUpdate((variables.size, currentSum), {
        val head :: tail = variables
        val hf :: tf = factors
        val newMin = min - head.dom.firstValue * hf
        val newMax = max - head.dom.lastValue * hf
        val trie = head.indices.
          map { i =>
            val v = head.dom.value(i)
            i -> zeroSum(tail, tf, newMin + v * hf, newMax + v * hf, currentSum + v * hf, nodes)
          } filter {
            _._2.nonEmpty
          } toSeq

        if (trie.isEmpty) {
          MDD0
        } else {
          new MDDn(MDD.newTrie(trie: _*), trie.map(_._1).toArray, trie.length)
        }
      })

    }
  }

}
