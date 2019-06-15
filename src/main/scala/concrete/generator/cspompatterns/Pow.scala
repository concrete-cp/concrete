package concrete.generator.cspompatterns

import cspom.CSPOM.SeqOperations
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}
import cspom.extension.MDDRelation
import cspom.util.{Infinitable, IntInterval, RangeSet}
import cspom.variable.IntExpression
import cspom.{CSPOM, CSPOMConstraint}
import mdd.MDD
import ConstraintCompiler._

object Pow extends ConstraintCompilerNoData {

  def functions = Functions("int_pow")

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM): Boolean = {
    constraint.result.isTrue
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM): Delta = {
    val args = constraint.arguments.map { case IntExpression(e) => e }

    val Seq(x, y, r) = args

    import IntExpression.implicits.iterable

    val mdd = try {
      pow(x.toSeq, y.toSeq, r.r)
    } catch {
      case e: ArithmeticException => throw new IllegalStateException(s"Could not handle $r = $x ^ $y", e)
    }

    //    println(constraint)
    //    println(mdd)

    val rDom = RangeSet(mdd.projectOne(2).map { i => IntInterval.singleton(i) })

    //    println(constraint)
    //    mdd.foreach(println)
    //    println(rDom)


    replaceCtr(constraint, args in new MDDRelation(mdd), problem) ++ replace(r, reduceDomain(r, rDom), problem)
  }

  def pow(xs: Seq[Int], ys: Seq[Int], rSpan: RangeSet[Infinitable]): MDD = {
    val lb = rSpan.span.lb
    val ub = rSpan.span.ub

    val trie = xs.map { x =>
      val xb = BigInt(x)

      val trie = ys.flatMap { y =>
        val rb = xb.pow(y)
        if (lb <= rb && ub >= rb) {
          if (rb.isValidInt) {
            Some(y -> MDD(Array(rb.intValue)))
          } else {
            throw new ArithmeticException(s"$rb: integer overflow")
          }
        } else {
          None
        }
      }


      x -> MDD.fromTrie(trie)
    }
    MDD.fromTrie(trie)
  }
}
