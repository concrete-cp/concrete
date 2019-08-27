package concrete.generator.cspompatterns

import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}
import cspom.extension.MDDRelation
import cspom.variable.IntExpression.implicits.iterable
import cspom.variable.{CSPOMConstant, CSPOMSeq, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}
import mdd._

import scala.collection.immutable.Queue
import scala.collection.mutable


object SlidingSum extends ConstraintCompilerNoData {

  def functions = Functions("slidingSum")

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = {
    constraint.nonReified && constraint.arguments.forall(_.fullyDefined)
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val Seq(CSPOMConstant(low: Int), CSPOMConstant(up: Int), CSPOMConstant(seq: Int), CSPOMSeq(args)) = constraint.arguments

    val vars = args.map(_.asInstanceOf[SimpleExpression[Int]]).toIndexedSeq

    //println(s"sizeR ${b.apply.lambda} ${b.apply.edges}")

    ConstraintCompiler.replaceCtr(constraint,
      CSPOM.IntSeqOperations(vars) in
        new MDDRelation(
          mdd(low, up, seq, vars.map(iterable(_).toSeq.map(cspom.util.Math.toIntExact))).reduce()
        ),
      problem)
  }

  def mdd(low: Int, up: Int, seq: Int, domains: IndexedSeq[Seq[Int]], k: Int = 0, queue: Queue[Int] = Queue.empty,
          nodes: mutable.HashMap[(Int, Queue[Int]), MDD] = new mutable.HashMap()): MDD = {
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

//  def main(arg: Array[String]): Unit = {
//    val domains = IndexedSeq.fill(10)(Seq(0,1))
//    val data = mdd(0, 2, 4, domains)
//
//    data.identify()

//    def toGML(mdd: MDD, ts: IdSet[MDD] = new IdSet()): Seq[Node] = {
//      ts.onceOrElse(mdd, {
//        if (mdd eq MDDLeaf) {
//          Seq(<node id={mdd.id.toString} label="l" />)
//        } else {
//          <node id={mdd.id.toString} label={mdd.id.toString} /> +:
//          mdd.children
//            .flatMap { case (i, submdd) =>
//              <edge source={mdd.id.toString} target={submdd.id.toString} label={i.toString} /> +:
//                toGML(submdd, ts)
//            }
//              .toSeq
//        }
//      }, Nil)
//    }

//    println(data)
//
//
//    val gml: Elem = <graphml xmlns="http://graphml.graphdrawing.org/xmlns"
//                             xmlns:svg="http://www.w3.org/2000/svg"
//                             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
//                             xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns
//                                graphml+svg.xsd">
//      <graph id="G" edgedefault="undirected">
//        {toGML(data)}
//      </graph>
//      </graphml>
//
//    val pp = new PrettyPrinter(80, 2)
//
//    println(pp.format(gml))


//  }

}
