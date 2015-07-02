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

final object SlidingSum extends ConstraintCompilerNoData {

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    constraint.function == 'slidingSum && constraint.result.isTrue
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    val Seq(CSPOMConstant(low: Int), CSPOMConstant(up: Int), CSPOMConstant(seq: Int), CSPOMSeq(args)) = constraint.arguments

    val vars = args.map(_.asInstanceOf[CSPOMVariable[Int]])

    //println(s"sizeR ${b.apply.lambda} ${b.apply.edges}")

    replaceCtr(constraint,
      CSPOM.SeqOperations(vars) in mdd(low, up, seq, vars.map(iterable).toIndexedSeq),
      problem)
  }

  def mdd(low: Int, up: Int, seq: Int, domains: IndexedSeq[Iterable[Int]], k: Int = 0, queue: Queue[Int] = Queue.empty,
          nodes: HashMap[(Int, Queue[Int]), MDD[Int]] = new HashMap()): MDD[Int] = {
    val current = queue.sum
    val nextDomains = domains.view.slice(k, k + seq - queue.size)
    if (current + nextDomains.map(_.min).sum > up) {
      MDD.empty
    } else if (current + nextDomains.map(_.max).sum < low) {
      MDD.empty
    } else if (k >= domains.length) {
      MDD.leaf
    } else {
      nodes.getOrElseUpdate((k, queue), {
        val nextQueue = if (queue.size >= seq) {
          queue.tail
        } else {
          queue
        }

        val children = domains(k).iterator map {
          i => i -> mdd(low, up, seq, domains, k + 1, nextQueue.enqueue(i), nodes)
        } filter {
          _._2.nonEmpty
        }

        new MDDNode(children.toMap)
      })
    }

  }

  def selfPropagation = false

}
