package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOMConstraint
import cspom.CSPOM
import cspom.extension.MDDNode
import cspom.variable.CSPOMVariable
import cspom.compiler.ConstraintCompiler
import cspom.variable.CSPOMSeq
import scala.collection.mutable.HashMap
import cspom.extension.Relation
import cspom.extension.MDD
import cspom.variable.CSPOMExpression
import cspom.variable.SimpleExpression

object Element extends ConstraintCompiler {

  type A = (SimpleExpression[_], CSPOMSeq[_], CSPOMVariable[_])

  override def constraintMatcher = {
    case CSPOMConstraint(r: SimpleExpression[_], 'element, Seq(array: CSPOMSeq[_], idx: CSPOMVariable[_]), _) => (r, array, idx)
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {
    val (res, vars, idx) = data
    val scope = vars.values.asInstanceOf[Seq[SimpleExpression[Any]]].toArray
    val range = vars.definedIndices

    val mdd = elementVar(scope, range)
    replaceCtr(c, CSPOM.table(mdd.reduce, false, idx +: res +: scope), p)
  }

  def elementVar[A <: B, B >: Int](scope: IndexedSeq[SimpleExpression[A]], range: Range): MDD[B] = {
    val cache = new HashMap[(Int, Int, A), MDD[A]]()

    val map: Seq[(B, MDD[B])] = for (i <- scope.indices) yield {

      val s: Map[B, MDD[B]] = scope(i).iterator.map {
        c => c -> elementVar(0, i, c, scope, cache).asInstanceOf[MDD[B]]
      } toMap

      range(i) -> new MDDNode(s)
    }

    new MDDNode(map.toMap)

  }

  private def elementVar[A](current: Int, b: Int, c: A, as: IndexedSeq[SimpleExpression[A]], cache: HashMap[(Int, Int, A), MDD[A]]): MDD[A] = {
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