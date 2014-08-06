package concrete.constraint.semantic

import concrete.UNSATObject
import concrete.Variable
import concrete.constraint.Constraint

sealed trait InvExpression {
  def present(i: Int): Boolean
  //def position: Int
  def filter(f: Int => Boolean): Boolean
  def singleton: Option[Int]
  def assign(i: Int): Boolean
}

final case class Const(c: Int) extends InvExpression {
  def present(i: Int) = i == c
  def filter(f: Int => Boolean): Boolean = if (f(c)) true else throw UNSATObject
  def singleton = Some(c)
  def assign(i: Int) = if (c == i) false else throw UNSATObject
}

final case class Var(v: Variable, position: Int) extends InvExpression {
  def present(i: Int) = v.dom.presentVal(i)
  def filter(f: Int => Boolean) = v.dom.filterValues(f)
  def singleton = {
    if (v.dom.size == 1) {
      Some(v.dom.firstValue)
    } else {
      None
    }
  }
  def assign(i: Int) = v.dom.assignVal(i)
}

/*
 * x[i] = j <=> y[j] = i
 * 
 */
class Inverse(
  val x: Array[InvExpression],
  val y: Array[InvExpression], vars: Array[Variable])
  extends Constraint(vars) {

  val k = x.length
  val l = y.length

  val xvariables = (0 until k).filter { i => x(i).isInstanceOf[Var] }
  val yvariables = (0 until l).filter { i => y(i).isInstanceOf[Var] }

  def checkValues(tuple: Array[Int]): Boolean = ???
  //  {
  //    x.indices.forall { i =>
  //      val j = tuple(i)
  //      j >= 0 && j < l && tuple(j + k) == i
  //    }
  //    y.indices.forall { i =>
  //      val j = tuple(i + k)
  //      j >= 0 && j < k && tuple(j) == i + k
  //    }
  //  }
  def simpleEvaluation: Int = 3

  def advise(pos: Int): Int = scopeSize

  def filter(varList: Seq[Int], x: Array[InvExpression], y: Array[InvExpression], ch: List[Int]): List[Int] = {
    var ch2 = ch
    for (i <- varList) {
      val vx = x(i)
      for (s <- vx.singleton) {
        if (y(s).assign(i)) {
          ch2 ::= y(s).asInstanceOf[Var].position
        }
      }
      if (vx.filter { j =>
        j >= 0 && j < l && y(j).present(i)
      }) {
        ch2 ::= vx.asInstanceOf[Var].position
      }
    }
    ch2
  }

  def revise() = {
    val ch = filter(xvariables, x, y, Nil)
    filter(yvariables, y, x, ch).distinct
  }
}