package cspfj.generator.constraint;

import scala.collection.mutable.HashMap
import cspfj.Problem
import cspfj.Variable
import cspfj.constraint.extension.MDD
import cspfj.constraint.extension.MDD0
import cspfj.constraint.extension.MDDLeaf
import cspfj.constraint.semantic.AllDifferent2C
import cspfj.constraint.semantic.AllDifferentBC
import cspfj.constraint.semantic.ZeroSum
import cspom.constraint.CSPOMConstraint
import cspom.constraint.GeneralConstraint
import cspfj.constraint.extension.MDDn
import cspfj.constraint.extension.ReduceableExt
import scala.annotation.tailrec

final class ZeroSumGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def generateGeneral(constraint: GeneralConstraint) = {
    val solverVariables = constraint.scope map cspom2cspfj toArray

    if (solverVariables exists { _.dom == null }) {
      false
    } else {
      val params = constraint.predicate.parameters map {
        p => p.split(", *").map(_.toInt)
      } getOrElse {
        Array.fill(solverVariables.length)(1)
      }
      //addConstraint(new ZeroSum(params, solverVariables));
      addConstraint(new ReduceableExt(solverVariables, zeroSum(solverVariables.toList, params.toList)))
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
    zeroSum(domains, factors, sum(domains, factors, _.min), sum(domains, factors, _.max))
  }

  def zeroSum(domains: List[Seq[Int]], factors: List[Int], min: Int, max: Int, currentSum: Int = 0,
    nodes: HashMap[(Int, Int), MDD] = new HashMap()): MDD = {
    if (domains.isEmpty) {
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
      nodes.getOrElseUpdate((domains.size, currentSum), {
        val head :: tail = domains
        val hf :: tf = factors
        val newMin = min - head.min * hf
        val newMax = max - head.max * hf
        val trie = head.iterator.
          map(i => i -> zeroSum(tail, tf, newMin + i * hf, newMax + i * hf, currentSum + i * hf, nodes)).
          filter(_._2.nonEmpty).
          toSeq

        if (trie.isEmpty) {
          MDD0
        } else {
          new MDDn(MDD.newTrie(trie: _*), trie.map(_._1).toArray, trie.length)
        }
      })

    }
  }

}
