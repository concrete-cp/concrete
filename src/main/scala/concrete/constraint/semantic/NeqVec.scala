package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.Domain
import concrete.Variable
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import scala.annotation.tailrec

final class NeqVec(x: Array[Variable], y: Array[Variable]) extends Constraint(x ++ y) //with Residues
//  with TupleEnumerator 
{
  require(x.length == y.length)

  val zip = x.zip(y)

  val size = arity / 2

  def checkValues(t: Array[Int]) = {
    t.view.splitAt(size).zipped.exists(_ != _)
  }

  def revise() = {
    if (singletonVector(x)) {
      singleFreeVariable(y, x) match {
        case Some((v, i)) => {
          y(v).dom.remove(i)
          entail()
          List(v + x.length)
        }
        case None => Nil
      }
    } else if (singletonVector(y)) {
      singleFreeVariable(x, y) match {
        case Some((v, i)) => {
          x(v).dom.remove(i)
          entail()
          List(v)
        }
        case None => Nil
      }
    } else Nil
  }

  /*
   * optimized v.forall(_.dom.size == 1)
   */
  @tailrec
  private def singletonVector(v: Array[Variable], i: Int = 0): Boolean =
    i >= v.length || (v(i).dom.size == 1 && singletonVector(v, i + 1))

  @tailrec
  private def singleFreeVariable(
    in: Array[Variable],
    withValues: Array[Variable],
    i: Int = 0,
    single: Option[(Int, Int)] = None): Option[(Int, Int)] = {

    if (i >= size) {
      single
    } else if (i == size - 1 && single.isEmpty) {
      val index = in(i).dom.index(withValues(i).dom.firstValue)
      if (index >= 0 && in(i).dom.present(index)) {
        Some((i, index))
      } else {
        entail()
        None
      }

    } else if (single.isDefined && in(i).dom.size > 1) {
      None
    } else {
      val index = in(i).dom.index(withValues(i).dom.firstValue)
      if (index >= 0 && in(i).dom.present(index)) {
        if (in(i).dom.size > 1) {
          singleFreeVariable(in, withValues, i + 1, Some((i, index)))
        } else {
          singleFreeVariable(in, withValues, i + 1, single)
        }
      } else {
        entail()
        None
      }
    }
  }

  override def toString = x.mkString("(", ", ", ")") + " /= " + y.mkString("(", ", ", ")")

  def advise(p: Int) = arity

  val simpleEvaluation = 2
}
