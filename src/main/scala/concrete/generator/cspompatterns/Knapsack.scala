package concrete.generator.cspompatterns

import scala.collection.mutable.HashMap

import cspom.CSPOM
import cspom.CSPOM.SeqOperations
import cspom.CSPOMConstraint
import cspom.compiler.ConstraintCompilerNoData
import cspom.extension.MDD
import cspom.util.Infinitable
import cspom.util.RangeSet
import cspom.variable.CSPOMSeq
import cspom.variable.IntExpression
import cspom.variable.IntExpression.implicits.iterable
import cspom.variable.SimpleExpression

import cspom.util.ContiguousIntRangeSet

import cspom.util.FiniteIntInterval

final object Knapsack extends ConstraintCompilerNoData {

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    constraint.function == 'knapsack && constraint.result.isTrue && constraint.arguments.forall(_.fullyDefined)
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    val Seq(IntExpression.constSeq(w), IntExpression.constSeq(p), CSPOMSeq(x), IntExpression(tw), IntExpression(tp)) = constraint.arguments

    val vars = x.map(_.asInstanceOf[SimpleExpression[Int]])

    //println(s"sizeR ${b.apply.lambda} ${b.apply.edges}")

    logger.info(s"Generating MDD for knapsack $constraint")

    val newConstraints = mdd(w.toIndexedSeq, p.toIndexedSeq, vars.toIndexedSeq, tw, tp)

    replaceCtr(constraint,
      newConstraints,
      problem)
  }

  def mdd(weights: IndexedSeq[Int], profits: IndexedSeq[Int],
          x: IndexedSeq[SimpleExpression[Int]],
          W: SimpleExpression[Int], P: SimpleExpression[Int]): Seq[CSPOMConstraint[Boolean]] = {
    import IntExpression.implicits.ranges
    val xd = x.map(IntExpression.implicits.ranges)

    val wMDD = mddSum(weights, xd, W)
    logger.info(wMDD.toString)
    val pMDD = mddSum(profits, xd, P)
    logger.info(pMDD.toString)

    logger.info("Computing intersection")
    val wpMDD1 = wMDD.insertDim(x.size, P.toSeq)
    val wpMDD2 = pMDD.insertDim(x.size - 1, W.toSeq)
    wpMDD1.boundIntersect(wpMDD2, 1000000)
      .map { its =>
        logger.info(s"Success ! $its")
        Seq((W +: P +: x) in its)
      }
      .recover {
        case _: IndexOutOfBoundsException =>
          logger.info("Too large, posting two constraints")
          Seq(
            (x :+ W) in wMDD,
            (x :+ P) in pMDD)
      }
      .get

  }

  def mddSum(factors: IndexedSeq[Int], x: IndexedSeq[RangeSet[Infinitable]], t: RangeSet[Infinitable]) = {
    zero(factors :+ -1, x :+ t)
  }

  def zero(factors: IndexedSeq[Int], x: IndexedSeq[RangeSet[Infinitable]]): MDD[Int] = {

    //val span = (x, factors).zipped.map { (x, f) => x.span * Finite(f) }.reduce(_ + _)

    val nodes = new HashMap[(Int, concrete.util.Interval), MDD[Int]]()
    val doms = x.map(new ContiguousIntRangeSet(_).toSeq)
    val spans = x.map(_.span).map {
      case FiniteIntInterval(l, u) => concrete.util.Interval(l, u)
    }
      .zip(factors)
      .map { case (x, f) => x * f }

    val span = spans.reduce(_ + _)

    def mdd(i: Int, span: concrete.util.Interval): MDD[Int] = {
      if (!span.contains(0)) {
        MDD.empty
      } else if (i >= x.size) {
        MDD.leaf
      } else nodes.getOrElseUpdate((i, span), {

        val f = factors(i)

        val rt = span.shrink(spans(i))

        MDD.node(doms(i).map { v =>
          v -> mdd(i + 1, rt + v * f)
        },
          true)
      })

    }

    mdd(0, span)

  }

  def selfPropagation = false

}
