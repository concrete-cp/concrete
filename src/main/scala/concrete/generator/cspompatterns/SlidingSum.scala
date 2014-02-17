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
import cspom.variable.IntConstant
import cspom.variable.CSPOMSeq
import cspom.variable.IntVariable
import cspom.extension.MDDNode
import scala.collection.immutable.Queue

final object SlidingSum extends ConstraintCompilerNoData {

  override def matchBool(constraint: CSPOMConstraint, problem: CSPOM) = {
    constraint.function == 'slidingSum
  }

  def compile(constraint: CSPOMConstraint, problem: CSPOM) = {
    val Seq(low: IntConstant, up: IntConstant, seq: IntConstant, args: CSPOMSeq[IntVariable]) = constraint.arguments

    val vars = args.values

    val b = new LazyMDD(Unit => mdd(low.value, up.value, seq.value, vars.toList))
    //println(s"sizeR ${b.apply.lambda} ${b.apply.edges}")
    replaceCtr(constraint,
      new CSPOMConstraint('extension, vars, constraint.params ++ Map("init" -> false, "relation" -> b)),
      problem)
  }

  def mdd(low: Int, up: Int, seq: Int, vars: List[IntVariable], queue: Queue[Int] = Queue.empty, current: Int = 0,
    nodes: HashMap[(List[IntVariable], Queue[Int], Int), MDD] = new HashMap()): MDD = {
    if (current > up) {
      EmptyMDD
    } else if (queue.size == seq && current < low) {
      EmptyMDD
    } else if (vars.isEmpty) {
      MDDLeaf
    } else {
      nodes.getOrElseUpdate((vars, queue, current), {

        val (c, nextQueue) = if (queue.size >= seq) {
          val (h, t) = queue.dequeue
          (current - h, t)
        } else {
          (current, queue)
        }

        val children = vars.head.domain map {
          i => i -> mdd(low, up, seq, vars.tail, nextQueue.enqueue(i), c + i)
        } filter {
          _._2.nonEmpty
        }

        if (children.isEmpty) {
          EmptyMDD
        } else {
          new MDDNode(children.toMap)
        }
      })
    }

  }

  def selfPropagation = false

  def main(args: Array[String]) {
    println(mdd(1, 3, 2, List.fill(5)(IntVariable.ofInterval(0, 5))))
  }
}
