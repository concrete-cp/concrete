package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.CSPOM.SeqOperations
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}
import cspom.extension.MDDRelation
import cspom.util.{ContiguousIntRangeSet, FiniteIntInterval, Infinitable, RangeSet}
import cspom.variable.IntExpression.implicits.iterable
import cspom.variable.{CSPOMSeq, IntExpression, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}
import mdd._

object Knapsack extends ConstraintCompilerNoData {

  def functions = Functions('knapsack)

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = {
    constraint.result.isTrue && constraint.arguments.forall(_.fullyDefined)
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val Seq(IntExpression.constSeq(w), IntExpression.constSeq(p), CSPOMSeq(x), IntExpression(tw), IntExpression(tp)) = constraint.arguments

    val vars = x.map(_.asInstanceOf[SimpleExpression[Int]])

    //println(s"sizeR ${b.apply.lambda} ${b.apply.edges}")

    logger.info(s"Generating MDD for knapsack $constraint")

    val newConstraints = mdd(w.toIndexedSeq, p.toIndexedSeq, vars.toIndexedSeq, tw, tp)
      .map(Seq(_))
      .getOrElse {
        logger.info("Too large, posting two constraints")
        Seq(
          CSPOMDriver.linear(tw +: vars, -1 +: w, "eq", 0),
          CSPOMDriver.linear(tp +: vars, -1 +: p, "eq", 0))
      }


    ConstraintCompiler.replaceCtr(constraint,
      newConstraints,
      problem)
  }

  def mdd(weights: IndexedSeq[Int], profits: IndexedSeq[Int],
          x: IndexedSeq[SimpleExpression[Int]],
          W: SimpleExpression[Int], P: SimpleExpression[Int]): Option[CSPOMConstraint[Boolean]] = {
    import IntExpression.implicits.ranges
    val xd = x.map(IntExpression.implicits.ranges)

    val wMDD = mddSum(weights, xd, W)
    // logger.info(wMDD.toString)
    val pMDD = mddSum(profits, xd, P)
    //  logger.info(pMDD.toString)

    //  logger.info("Computing intersection")
    val wpMDD1 = wMDD.insertDim(0, P.toSeq)
    val wpMDD2 = pMDD.insertDim(1, W.toSeq)

    logger.info(s"Intersection of $wpMDD1 and $wpMDD2...")
    if (wpMDD1.vertices().toDouble * wpMDD2.vertices() < 2e10) {
      val its = wpMDD1.intersect(wpMDD2)
      logger.info(s"Success ! $its")
      Some((P +: W +: x) in new MDDRelation(its))
    } else {
      None
    }


  }

  def mddSum(factors: IndexedSeq[Int], x: IndexedSeq[RangeSet[Infinitable]], t: RangeSet[Infinitable]): MDD = {
    zero(-1 +: factors, t +: x)
  }

  def zero(factors: IndexedSeq[Int], x: IndexedSeq[RangeSet[Infinitable]]): MDD = {

    //val span = (x, factors).zipped.map { (x, f) => x.span * Finite(f) }.reduce(_ + _)

    val nodes = new JavaMap[(Int, concrete.util.Interval), MDD]()
    val doms = x.map(new ContiguousIntRangeSet(_).toSeq)
    val spans = x.map(_.span).map {
      case FiniteIntInterval(l, u) => concrete.util.Interval(l, u)
    }
      .zip(factors)
      .map { case (x, f) => x * f }

    val span = spans.reduce(_ + _)

    def mdd(i: Int, span: concrete.util.Interval): MDD = {
      if (!span.contains(0)) {
        MDD0
      } else if (i >= x.size) {
        MDDLeaf
      } else nodes.getOrElseUpdate((i, span), {

        val f = factors(i)

        val rt = span.shrink(spans(i))

        MDD.fromTrie(
          doms(i)
            .map { v =>
              v -> mdd(i + 1, rt + v * f)
            }
            .filter(_._2.nonEmpty))
      })

    }

    mdd(0, span)

  }

  def selfPropagation = false

}
