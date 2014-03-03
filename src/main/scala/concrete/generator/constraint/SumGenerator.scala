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
import Generator._
import concrete.constraint.semantic.FilterSum
import cspom.variable.CSPOMSeq
import concrete.constraint.semantic.FilterSum
import concrete.constraint.semantic.FilterSum
import concrete.util.Interval
import cspom.variable.CSPOMConstant

final object SumGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(CSPOMSeq(vars, _, _), const: CSPOMConstant[Int]) = constraint.arguments //map cspom2concreteVar

    val c = const.value

    val solverVariables = vars map cspom2concreteVar

    val Some(mode: FilterSum.Value) = constraint.params.get("mode").collect { case m: String => FilterSum.withName(m) }

    val params = constraint.params.get("coefficients") match {
      case Some(p: Seq[Int]) => p.toArray
      case None => Array.fill(solverVariables.length)(1)
      case _ => throw new IllegalArgumentException("Parameters for zero sum must be a sequence of integer values")
    }

    undefinedVar(solverVariables: _*) match {
      case Seq() => go(c, params, solverVariables, mode)
      case Seq(uv) if mode == FilterSum.SumEQ =>
        val min = (solverVariables zip params).map { case (v, p) => if (v eq uv) 0 else v.dom.firstValue * p }.sum
        val max = (solverVariables zip params).map { case (v, p) => if (v eq uv) 0 else v.dom.lastValue * p }.sum
        val factor = params(solverVariables.indexOf(uv))
        Generator.restrictDomain(uv, (Interval(min, max) / -factor).allValues)
        go(c, params, solverVariables, mode)
      case s => None
    }

  }

  def go(c: Int, params: Array[Int], solverVariables: Seq[Variable], mode: FilterSum.Value) =
    Some(Seq(new Sum(c, params, solverVariables.toArray, mode)))

  //      result match {
  //        case Const(c) => Some(Seq(new Sum(c, params, solverVariables.toArray, mode)))
  ////        case Var(v) => {
  //          if (v.dom.undefined) {
  //            val min = (solverVariables zip params).map { case (v, p) => v.dom.firstValue * p }.sum
  //            val max = (solverVariables zip params).map { case (v, p) => v.dom.lastValue * p }.sum
  //            Generator.restrictDomain(v, min to max)
  //          }
  //          Some(Seq(new Sum(0, -1 +: params, (v +: solverVariables).toArray, mode)))
  //        }
  //        case _ => throw new FailedGenerationException("Variable or constant expected, found " + result)
  //      }

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
