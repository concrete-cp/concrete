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
import SimpleExpression.iterable
import scala.collection.mutable.WeakHashMap

object Element extends ConstraintCompiler {

  type A = (SimpleExpression[_], CSPOMSeq[_], SimpleExpression[_])

  override def constraintMatcher = {
    case CSPOMConstraint(r: SimpleExpression[_], 'element, Seq(array: CSPOMSeq[_], idx: SimpleExpression[_]), _) => (r, array, idx)
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, data: A) = {
    val (res, vars, idx) = data
    val scope = vars.values.asInstanceOf[Seq[SimpleExpression[Any]]].toArray
    val range = vars.definedIndices

    logger.info("will generate for " + scope.toSeq)
    val mdd = elementVar(scope.map(_.toSeq), range)
    //mdd.foreach(println)
    replaceCtr(
      c,
      CSPOM.table(
        mdd.asInstanceOf[MDD[Any]], false, idx +: res +: scope),
      p)
  }

  val laziness = new HashMap[(IndexedSeq[Seq[Any]], Range), MDD[Any]]()

  def elementVar(scope: IndexedSeq[Seq[Any]], range: Range): MDD[Any] =
    laziness.getOrElseUpdate((scope, range), {
      logger.info("generation")
      val cache = new HashMap[(Int, Int, Any), MDD[Any]]()

      val map: Seq[(Int, MDD[Any])] = for (i <- scope.indices) yield {

        val s: Map[Any, MDD[Any]] = scope(i).iterator
          .map {
            c => c -> elementVar(0, i, c, scope, cache) //.asInstanceOf[MDD[B]]
          }
          .toMap

        range(i) -> new MDDNode(s)
      }

      new MDDNode(map.toMap)

    })

  private def elementVar(current: Int, b: Int, c: Any,
    as: IndexedSeq[Seq[Any]], cache: HashMap[(Int, Int, Any), MDD[Any]]): MDD[Any] = {
    if (current >= as.length) {
      MDD.leaf
    } else cache.getOrElseUpdate((current, b, c), {
      if (current == b) {
        new MDDNode(Map(c -> elementVar(current + 1, b, c, as, cache)))
      } else {
        new MDDNode(as(current).iterator
          .map {
            v => v -> elementVar(current + 1, b, c, as, cache)
          }
          .toMap)
      }
    })

  }

  def selfPropagation = false
}