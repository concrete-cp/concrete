package concrete.generator.cspompatterns

import scala.collection.mutable.HashMap

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompiler
import cspom.compiler.ConstraintCompilerNoData
import cspom.extension.MDD
import cspom.extension.MDDNode
import cspom.extension.Relation
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.CSPOMVariable
import cspom.variable.SimpleExpression
import cspom.variable.IntVariable.iterable

object Element extends ConstraintCompiler {

  type A = (SimpleExpression[Int], CSPOMSeq[Int], CSPOMVariable[Int])

  override def constraintMatcher = {
    case CSPOMConstraint(r: SimpleExpression[Int], 'element, Seq(array: CSPOMSeq[Int], idx: CSPOMVariable[Int]), _) => (r, array, idx)
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {
    val (res, vars, idx) = data
    val scope = vars.values.asInstanceOf[Seq[SimpleExpression[Int]]].toArray
    val range = vars.definedIndices

    val mdd = elementVar(scope, range)
    replaceCtr(c, CSPOM.table[Int](mdd.reduce, false, idx +: res +: scope), p)
  }

  def elementVar(scope: IndexedSeq[SimpleExpression[Int]], range: Range): MDD[Int] = {
    val cache = new HashMap[(Int, Int, Int), MDD[Int]]()

    val map: Seq[(Int, MDD[Int])] = for (i <- scope.indices) yield {

      val s: Map[Int, MDD[Int]] = scope(i).iterator.map {
        c => c -> elementVar(0, i, c, scope, cache) //.asInstanceOf[MDD[B]]
      } toMap

      range(i) -> new MDDNode(s)
    }

    new MDDNode(map.toMap)

  }

  private def elementVar(current: Int, b: Int, c: Int, 
      as: IndexedSeq[SimpleExpression[Int]], cache: HashMap[(Int, Int, Int), MDD[Int]]): MDD[Int] = {
    if (current >= as.length) {
      MDD.leaf
    } else cache.getOrElseUpdate((current, b, c), {
      if (current == b) {
        new MDDNode(Map(c -> elementVar(current + 1, b, c, as, cache)))
      } else {
        new MDDNode(as(current).iterator.map {
          v => v -> elementVar(current + 1, b, c, as, cache)
        } toMap)
      }
    })

  }

  def selfPropagation = false
}