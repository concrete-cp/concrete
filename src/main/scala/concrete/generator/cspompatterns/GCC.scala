package concrete.generator.cspompatterns

import concrete.SumBuilder
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}
import cspom.extension.MDDRelation
import cspom.util.FiniteIntInterval
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint}
import mdd.{MDD, MDD0, MDDLeaf}
import CSPOM._

import scala.collection.mutable

/**
  * @author vion
  */
object GCC extends ConstraintCompilerNoData {

  private val gccAsMDD = false

  def functions = Functions("gccExact", "gccMinMax")

  def matchBool(c: CSPOMConstraint[_], p: CSPOM): Boolean = c.result.isTrue

  def compile(c: CSPOMConstraint[_], p: CSPOM): Delta = {

    c.function match {
      case "gccExact" =>
        val CSPOMConstraint(_, _, Seq(SimpleExpression.simpleSeq(vars), CSPOMConstant(closed: Boolean), CSPOMConstant.seq(values), IntExpression.simpleSeq(occ)), params) = c

        //        val constraints =
        val domains = vars.toList.map(
          SimpleExpression.iterable(_)
            .map(concrete.util.Math.any2Int)
            .toList)

        val vals = values.map(concrete.util.Math.any2Int)

        val bounds = (vals lazyZip occ).map {
          case (v, o) =>
            val FiniteIntInterval(lb, ub) = IntExpression.span(o)
            v -> (lb to ub)
        }
          .toMap

        val constraints = if (gccAsMDD) {
          Seq(
            CSPOMConstraint("extension")(vars ++ occ: _*)
              .withParam("init" -> false, "relation" -> new MDDRelation(mdd(domains, vals.toIndexedSeq, bounds)))
          )
        } else {
          for ((v, o) <- vals zip occ) yield {
            CSPOMConstraint(o)("occurrence")(CSPOMConstant(v), vars)
          }
        }

        logger.debug(constraints.toString)

        val closedCons: Seq[CSPOMConstraint[_]] = if (closed) {
          val in = closedConstraints(vars, vals)
          val implied = occ.map(SumBuilder(_)).reduce(_ + _) === vars.length
          implied +: in
        } else {
          val implied = occ.map(SumBuilder(_)).reduce(_ + _) <= vars.length
          Seq(implied)
        }

        ConstraintCompiler.replaceCtr(c, constraints ++: closedCons, p)

      case "gccMinMax" =>
        val CSPOMConstraint(_, _, Seq(vars: CSPOMSeq[_], CSPOMConstant(closed: Boolean), CSPOMConstant.seq(values), IntExpression.simpleSeq(occMin), IntExpression.simpleSeq(occMax)), params) = c

        val vals = values.map(concrete.util.Math.any2Int)

        val constraints =
          for {(v, min, max) <- (vals lazyZip occMin lazyZip occMax).toSeq
               c <- Seq(
                 CSPOMConstraint("atLeast")(min, CSPOMConstant(v), vars),
                 CSPOMConstraint("atMost")(max, CSPOMConstant(v), vars))
          } yield c

        val closedCons: Seq[CSPOMConstraint[_]] = if (closed) {
          val in = closedConstraints(vars, vals)
          val implied = Seq(
            occMin.map(SumBuilder(_)).reduce(_ + _) <= vars.length,
            occMax.map(SumBuilder(_)).reduce(_ + _) >= vars.length)

          implied ++: in
        } else {
          Seq()
        }

        ConstraintCompiler.replaceCtr(c, constraints ++ closedCons, p)
    }

  }

  def mdd(x: List[List[Int]], cover: IndexedSeq[Int], bounds: Map[Int, Range], counts: Map[Int, Int] = Map().withDefaultValue(0),
          cache: mutable.Map[(Int, Map[Int, Int]), MDD] = new mutable.HashMap()): MDD = {
    cache.getOrElseUpdate((x.size, counts), {
      x match {
        case Nil => countsMDD(counts, cover)
        case xHead :: xTail =>
          val unfeasible = bounds.iterator.map {
            case (i, r) => r.head - counts(i)
          }.sum > x.size
          if (unfeasible) {
            MDD0
          } else {
            val mandatory = xHead.filter(i => bounds.get(i).exists(b => counts(i) + x.size == b.head))
            mandatory match {
              case Nil =>
                MDD.fromTrie {
                  xHead
                    .filter(i => bounds.get(i).forall(b => counts(i) < b.last))
                    .map {
                      i =>
                        val nextSoFar = if (bounds.contains(i)) {
                          counts.updated(i, counts(i) + 1)
                        } else {
                          counts
                        }
                        i -> mdd(xTail, cover, bounds, nextSoFar, cache)
                    }
                    .filter(_._2.nonEmpty)
                }

              case i :: Nil =>
                MDD.fromTrie(
                  Seq(i -> mdd(xTail, cover, bounds, counts.updated(i, counts(i) + 1), cache))
                    .filter(_._2.nonEmpty)
                )

              case _ =>
                MDD0
            }
          }
      }
    }
    )
  }

  def main(args: Array[String]): Unit = {
    val breaks = List.fill(14)((0 until 20).toList)
    val cover = IndexedSeq(0, 9, 11, 13, 15, 17, 19)
    val bounds = cover.map(i => i -> (0 to 2)).toMap

    val m = mdd(breaks, cover, bounds)
    println(m)
    val r = m.reduce()
    println(r)


    //r.take(10).foreach(println)
  }

  private def closedConstraints(vars: Seq[CSPOMExpression[_]], values: Seq[Int]) = {
    for (v <- vars) yield {
      CSPOMConstraint("in")(v, CSPOM.constantSeq(values))
    }
  }

  private def countsMDD[T](counts: Map[T, Int], cover: IndexedSeq[T]): MDD = {
    if (cover.isEmpty) {
      MDDLeaf
    } else {
      MDD.fromTrie(Seq(counts(cover.head) -> countsMDD(counts, cover.tail)))
    }
  }
}