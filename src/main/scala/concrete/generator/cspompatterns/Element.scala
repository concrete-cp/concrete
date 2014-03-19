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

object Element extends ConstraintCompiler {

  type A = (CSPOMVariable[_], CSPOMSeq[_], CSPOMVariable[_])

  override def constraintMatcher = {
    case CSPOMConstraint(r: CSPOMVariable[_], 'element, Seq(array: CSPOMSeq[_], idx: CSPOMVariable[_]), _) => (r, array, idx)
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {
    val (res, vars, idx) = data
    val scope = vars.values.asInstanceOf[Seq[SimpleExpression[A]]].toArray
    val range = vars.definedIndices
    replaceCtr(c, CSPOM.table(elementVar(scope, range), false, idx +: res +: scope), p)
  }

  def elementVar[A <: B, B >: Int](scope: Array[SimpleExpression[A]], range: Range): Relation[B] = {
    val cache = new HashMap[(Int, Int, A), MDD[A]]()

    val map: Seq[(B, MDD[B])] = for (i <- scope.indices) yield {

      val s: Seq[(B, MDD[B])] = scope(i).domain.map {
        c => c -> elementVar(0, i, c, scope, cache).asInstanceOf[MDD[B]]
      }

      range(i) -> new MDDNode(s.toMap)
    }

    new MDDNode(map.toMap)

  }

  private def elementVar[A](current: Int, b: Int, c: A, as: Array[CSPOMVariable[A]], cache: HashMap[(Int, Int, A), MDD[A]]): MDD[A] = {
    if (current >= as.length) {
      MDD.leaf
    } else cache.getOrElseUpdate((current, b, c), {
      if (current == b) {
        new MDDNode(Map(c -> elementVar(current + 1, b, c, as, cache)))
      } else {
        new MDDNode(as(current).domain.map {
          v => v -> elementVar(current + 1, b, c, as, cache)
        } toMap)
      }
    })

  }

  def selfPropagation = false
}