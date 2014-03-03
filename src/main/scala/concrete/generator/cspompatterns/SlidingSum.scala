package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.CSPOM
import cspom.extension.LazyMDD
import cspom.extension.EmptyMDD
import scala.collection.mutable.HashMap
import cspom.extension.MDD
import cspom.extension.MDDLeaf
import concrete._
import concrete.generator.constraint.Const
import concrete.generator.constraint.Generator
import concrete.generator.constraint.Sequence
import concrete.generator.constraint.Var
import cspom.compiler.ConstraintCompilerNoData
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMSeq
import cspom.variable.IntVariable
import cspom.extension.MDDNode
import scala.collection.immutable.Queue

final object SlidingSum extends ConstraintCompilerNoData {

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    constraint.function == 'slidingSum
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    val Seq(low: CSPOMConstant[Int], up: CSPOMConstant[Int], seq: CSPOMConstant[Int], args: CSPOMSeq[Int]) = constraint.arguments

    val vars = args.values.map(_.asInstanceOf[IntVariable])

    val b = new LazyMDD(Unit => mdd(low.value, up.value, seq.value, vars.toArray))
    //println(s"sizeR ${b.apply.lambda} ${b.apply.edges}")
    replaceCtr(constraint,
      CSPOMConstraint('extension, vars, constraint.params ++ Map("init" -> false, "relation" -> b)),
      problem)
  }

  def mdd(low: Int, up: Int, seq: Int, vars: Array[IntVariable], k: Int = 0, queue: Queue[Int] = Queue.empty,
    nodes: HashMap[(Int, Queue[Int]), MDD] = new HashMap()): MDD = {
    val current = queue.sum
    val nextVars = vars.view.slice(k, k + seq - queue.size)
    if (current + nextVars.map(_.domain.min).sum > up) {
      EmptyMDD
    } else if (current + nextVars.map(_.domain.max).sum < low) {
      EmptyMDD
    } else if (k >= vars.length) {
      MDDLeaf
    } else {
      nodes.getOrElseUpdate((k, queue), {
        val nextQueue = if (queue.size >= seq) {
          queue.tail
        } else {
          queue
        }

        val children = vars(k).domain.iterator map {
          i => i -> mdd(low, up, seq, vars, k + 1, nextQueue.enqueue(i), nodes)
        } filter {
          _._2.nonEmpty
        }

        new MDDNode(children.toMap)
      })
    }

  }

  def selfPropagation = false

}
