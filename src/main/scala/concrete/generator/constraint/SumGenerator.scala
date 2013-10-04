package concrete.generator.constraint;

import scala.collection.mutable.HashMap
import concrete.Problem
import concrete.Variable
import concrete.constraint.extension.MDD
import concrete.constraint.extension.MDD0
import concrete.constraint.extension.MDDLeaf
import concrete.constraint.semantic.AllDifferent2C
import concrete.constraint.semantic.AllDifferentBC
import concrete.constraint.semantic.Sum
import cspom.CSPOMConstraint
import concrete.constraint.extension.MDDn
import concrete.constraint.extension.ReduceableExt
import scala.annotation.tailrec
import concrete.generator.FailedGenerationException

final class SumGenerator(problem: Problem) extends AbstractGenerator(problem) {
  //  override def gen(constraint: CSPOMConstraint) = {
  //    val Seq(solverVariables) = constraint.arguments map cspom2concreteSeqVar
  //
  //    if (solverVariables exists { _.dom.undefined }) {
  //      false
  //    } else {
  //      val params = constraint.params.get("coefficients") match {
  //        case Some(p: Seq[Int]) => p.toArray
  //        case None => Array.fill(solverVariables.length)(1)
  //        case _ => throw new IllegalArgumentException("Parameters for zero sum must be a sequence of integer values")
  //      }
  //      addConstraint(new Sum(0, params, solverVariables.toArray));
  //      //addConstraint(new ReduceableExt(solverVariables, zeroSum(solverVariables.toList, params.toList)))
  //      true;
  //    }
  //  }

  override def genFunctional(constraint: CSPOMConstraint, result: C2Conc) = {
    val solverVariables = constraint.arguments map cspom2concreteVar

    if (result.undefined || solverVariables.exists(_.dom.undefined)) {
      false
    } else {
      val params = constraint.params.get("coefficients") match {
        case Some(p: Seq[Int]) => p.toArray
        case None => Array.fill(solverVariables.length)(1)
        case _ => throw new IllegalArgumentException("Parameters for zero sum must be a sequence of integer values")
      }
      result match {
        case C2C(c) => addConstraint(new Sum(c, params, solverVariables.toArray));
        case C2V(v) => addConstraint(new Sum(0, -1 +: params, (v +: solverVariables).toArray))
        case _ => throw new FailedGenerationException("Variable or constant expected, found " + result)
      }

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
