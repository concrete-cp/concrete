package concrete.generator.cspompatterns

import cspom.CSPOM.SeqOperations
import cspom.compiler.ConstraintCompilerNoData
import cspom.extension.MDDRelation
import cspom.variable.IntExpression
import cspom.{CSPOM, CSPOMConstraint}
import mdd.MDD

final object Pow extends ConstraintCompilerNoData {

  override def matchBool(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    constraint.function == 'int_pow && constraint.result.isTrue
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM) = {
    val args = constraint.arguments.map { case IntExpression(e) => e }

    val Seq(x, y, r) = args

    import IntExpression.implicits.iterable

    val mdd = pow(x.toSeq, y.toSeq, r.head, r.last)

    replaceCtr(constraint, args in new MDDRelation(mdd), problem)
  }

  def pow(xs: Seq[Int], ys: Seq[Int], rmin: Int, rmax: Int): MDD = {
    val trie = xs.map { x =>
      val xb = BigInt(x)

      val trie = ys.flatMap { y =>
        val rb = xb.pow(y)
        if (rmin <= rb && rb <= rmax) {
          if (rb.isValidInt) {
            Some(y -> MDD(Array(rb.intValue())))
          } else {
            throw new ArithmeticException("integer overflow")
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
