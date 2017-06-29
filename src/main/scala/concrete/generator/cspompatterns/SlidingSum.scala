package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompilerNoData
import cspom.extension.MDDRelation
import cspom.variable.IntExpression.implicits.iterable
import cspom.variable.{CSPOMConstant, CSPOMSeq, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}
import mdd.{JavaMap, MDD, MDD0, MDDLeaf}

import scala.collection.immutable.Queue

final object SlidingSum extends ConstraintCompilerNoData {

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    constraint.function == 'slidingSum && constraint.result.isTrue && constraint.arguments.forall(_.fullyDefined)
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    val Seq(CSPOMConstant(low: Int), CSPOMConstant(up: Int), CSPOMConstant(seq: Int), CSPOMSeq(args)) = constraint.arguments

    val vars = args.map(_.asInstanceOf[SimpleExpression[Int]])

    //println(s"sizeR ${b.apply.lambda} ${b.apply.edges}")

    replaceCtr(constraint,
      CSPOM.IntSeqOperations(vars) in new MDDRelation(mdd(low, up, seq, vars.map(iterable(_).toSeq).toIndexedSeq)),
      problem)
  }

  def mdd(low: Int, up: Int, seq: Int, domains: IndexedSeq[Seq[Int]], k: Int = 0, queue: Queue[Int] = Queue.empty,
          nodes: JavaMap[(Int, Queue[Int]), MDD] = new JavaMap()): MDD = {
    val current = queue.sum
    val nextDomains = domains.view.slice(k, k + seq - queue.size)
    if (current + nextDomains.map(_.min).sum > up) {
      MDD0
    } else if (current + nextDomains.map(_.max).sum < low) {
      MDD0
    } else if (k >= domains.length) {
      MDDLeaf
    } else {
      nodes.getOrElseUpdate((k, queue), {
        val nextQueue = if (queue.size >= seq) {
          queue.tail
        } else {
          queue
        }

        val children = domains(k)
          .map(i => i -> mdd(low, up, seq, domains, k + 1, nextQueue.enqueue(i), nodes))
          .filter(_._2.nonEmpty)

        MDD.fromTrie(children)
      })
    }

  }

  def selfPropagation = false

}
