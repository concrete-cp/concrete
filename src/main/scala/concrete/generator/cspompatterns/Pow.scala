package concrete.generator.cspompatterns

import cspom.CSPOM.SeqOperations
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.{ConstraintCompilerNoData, Delta, Functions}
import cspom.extension.MDDRelation
import cspom.util.{Infinitable, IntInterval, RangeSet}
import cspom.variable.IntExpression
import cspom.variable.IntExpression.implicits.iterable
import cspom.{CSPOM, CSPOMConstraint}
import mdd.MDD

import scala.util.{Failure, Try}

object Pow extends ConstraintCompilerNoData {

  def functions = Functions("int_pow")

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = {
    constraint.result.isTrue
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val args = constraint.arguments.map { case IntExpression(e) => e }

    val Seq(x, y, r) = args

    val mdd = Try {
      pow(iterable(x).toSeq, iterable(y).toSeq, iterable(r).r)
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

    //    println(constraint)
    //    mdd.foreach(println)
    //    println(rDom)


    replaceCtr(constraint, args in new MDDRelation(mdd), problem) ++ replace(r, reduceDomain(r, rDom), problem)
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
