package cspfj.constraint.semantic;

import cspfj.constraint.Constraint
import cspfj.Domain
import cspfj.Variable
import cspfj.constraint.Residues
import cspfj.constraint.TupleEnumerator
import scala.annotation.tailrec

final class NeqVec(x: Array[Variable], y: Array[Variable]) extends Constraint(x ++ y) //with Residues
//  with TupleEnumerator 
{
  require(x.length == y.length)

  val zip = x.zip(y)

  def checkValues(t: Array[Int]) = {
    t.view.splitAt(arity / 2).zipped.exists(_ != _)
  }

  def revise() = {
    if (singletonVector(x)) {
      singleFreeVariable(y, x) match {
        case Some((v, i)) => {
          v.dom.remove(i)
          entail()
          true
        }
        case None => false
      }
    } else if (singletonVector(y)) {
      singleFreeVariable(x, y) match {
        case Some((v, i)) => {
          v.dom.remove(i)
          entail()
          true
        }
        case None => false
      }
    } else false
  }

  /*
   * optimized v.forall(_.dom.size == 1)
   */
  @tailrec
  private def singletonVector(v: Array[Variable], i: Int = 0): Boolean =
    i >= v.length || (v(i).dom.size == 1 && singletonVector(v, i + 1))

  @tailrec
  private def singleFreeVariable(in: Array[Variable], withValues: Array[Variable], i: Int = 0, single: Option[(Variable, Int)] = None): Option[(Variable, Int)] = {
    if (i >= arity / 2) single
    else if (single.isDefined && in(i).dom.size > 1) None
    else {
      val index = in(i).dom.index(withValues(i).dom.firstValue)
      if (index >= 0 && in(i).dom.present(index)) {
        if (in(i).dom.size > 1) singleFreeVariable(in, withValues, i + 1, Some(in(i), index))
        else singleFreeVariable(in, withValues, i + 1, single)
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
