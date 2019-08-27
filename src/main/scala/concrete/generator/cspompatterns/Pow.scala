package concrete.generator.cspompatterns

import cspom.compiler.ConstraintCompiler._
import cspom.compiler.{ConstraintCompilerNoData, Delta, Functions}
import cspom.extension.MDDRelation
import cspom.util.{Infinitable, IntInterval, RangeSet}
import cspom.variable.{IntExpression, SimpleExpression}
import cspom.variable.IntExpression.implicits.iterable
import cspom.{CSPOM, CSPOMConstraint}
import mdd.MDD

import scala.util.{Failure, Try}

object Pow extends ConstraintCompilerNoData {

  def functions = Functions("pow")

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean =
    true

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val args = constraint.arguments.map { case IntExpression(e) => e }

    val IntExpression(r) = constraint.result

    val Seq(x, y) = args


    val mdd = Try {
      pow(
        iterable(x).toSeq.map(cspom.util.Math.toIntExact),
        iterable(y).toSeq.map(cspom.util.Math.toIntExact),
        iterable(r).r)
    }
      .recoverWith {
        case e: ArithmeticException =>
          Failure(new IllegalStateException(s"Could not handle $r = $x ^ $y", e))
      }
      .get
      .reduce()

    //    println(constraint)
    //    println(mdd)

    val rDom = RangeSet(mdd.projectOne(2).map { i => IntInterval.singleton(i) })


    val newArgs = CSPOM.IntSeqOperations(Seq(x, y, r))

    replaceCtr(constraint, newArgs in new MDDRelation(mdd), problem) ++ replace(r, reduceDomain(r, rDom), problem)
  }

  private def pow(xs: Seq[Int], ys: Seq[Int], rSpan: RangeSet[Infinitable]): MDD = {
    val lb = rSpan.span.lb
    val ub = rSpan.span.ub

    val trie = xs.map { x =>
      val xb = BigInt(x)

      val trie = for {
        y <- ys
        if y >= 0
        rb = xb.pow(y)
        if lb <= rb && ub >= rb
      } yield {
        y -> MDD(Array(cspom.util.Math.toIntExact(rb)))
      }

      x -> MDD.fromTrie(trie)
    }
    MDD.fromTrie(trie)
  }
}
