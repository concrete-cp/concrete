package concrete.constraint.semantic;

import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.Variable
import concrete.constraint.TupleEnumerator
import concrete.UNSATException

final class LexLeq(x: Array[Variable], y: Array[Variable]) extends Constraint(x ++ y) {
  val size = x.length
  require(size == y.length)

  def revise() = {
    init()
    change
  }

  private def groundEq(i: Int) = {
    x(i).dom.size == 1 && y(i).dom.size == 1 && x(i).dom.firstValue == y(i).dom.firstValue
  }

  def checkValues(t: Array[Int]) = throw new UnsupportedOperationException

  private def notAlwaysLt(i: Int) = min(x(i)) <= max(y(i))

  private def alwaysLeq(i: Int) = max(x(i)) <= min(y(i))

  private def alwaysLt(i: Int) = max(x(i)) < min(y(i))

  private def checkLex(i: Int) = {
    if (i == size - 1) alwaysLeq(i)
    else alwaysLt(i)
  }

  var consistent = false
  var alpha = -1
  var beta = -1
  var change: List[Int] = Nil

  private def init() {
    change = Nil
    consistent = false
    var i = 0
    while (i < size && groundEq(i)) i += 1

    if (i == size) {
      consistent = true
    } else {
      alpha = i

      if (checkLex(i)) {
        consistent = true
        return
      }

      beta = -1
      while (i != size && notAlwaysLt(i)) {
        if (x(i).dom.firstValue == y(i).dom.lastValue) {
          if (beta == -1) beta = i
        } else {
          beta = -1
        }
        i += 1
      }

      if (i == size) beta = Integer.MAX_VALUE
      else if (beta == -1) beta = i
      if (alpha >= beta) throw UNSATException.e
      gacLexLeq(alpha)
    }
  }

  private def gacLexLeq(i: Int) {
    if (i >= beta || consistent) return
    if (i == alpha && i + 1 == beta) {
      acLt(i)
      if (checkLex(i)) {
        consistent = true
        return
      }
    }
    if (i == alpha && i + 1 < beta) {
      acLeq(i)
      if (checkLex(i)) {
        consistent = true
        return
      }
      if (groundEq(i)) updateAlpha(i + 1)
    }
    if (alpha < i && i < beta) {
      if ((i == beta - 1 && min(x(i)) == max(y(i))) || alwaysLt(i))
        updateBeta(i + 1)
    }
  }

  private def removeGt(value: Int, v: Variable) = {
    val dom = v.dom
    val lb = dom.closestGt(value)
    lb >= 0 && dom.removeFrom(lb)
  }

  private def removeLt(value: Int, v: Variable) = {
    val dom = v.dom;
    val ub = dom.closestLt(value)
    ub >= 0 && dom.removeTo(ub)
  }

  private def removeGeq(value: Int, v: Variable) = {
    val dom = v.dom
    val lb = dom.closestGeq(value)
    lb >= 0 && dom.removeFrom(lb)
  }

  private def removeLeq(value: Int, v: Variable) = {
    val dom = v.dom;
    val ub = dom.closestLeq(value)
    ub >= 0 && dom.removeTo(ub)
  }

  private def min(v: Variable) = v.dom.firstValue

  private def max(v: Variable) = v.dom.lastValue

  private def acLt(i: Int) {
    if (removeLeq(min(x(i)), y(i))) {
      change ::= i + size
    }
    if (removeGeq(max(y(i)), x(i))) {
      change ::= i
    }
  }
  private def acLeq(i: Int) {
    if (removeLt(min(x(i)), y(i))) {
      change ::= i + size
    }
    if (removeGt(max(y(i)), x(i))) {
      change ::= i
    }
  }

  private def updateAlpha(i: Int) {
    if (i == beta) throw UNSATException.e
    if (i == size) {
      consistent = true
      return
    }
    if (!groundEq(i)) {
      alpha = i
      gacLexLeq(i)
    } else updateAlpha(i + 1)

  }

  private def updateBeta(i: Int) {
    if (i + 1 == alpha) throw UNSATException.e
    if (min(x(i)) < max(y(i))) {
      beta = i + 1
      if (notAlwaysLt(i)) gacLexLeq(i)
    } else if (min(x(i)) == max(y(i))) updateBeta(i - 1)
  }

  def advise(p: Int) = size
  def simpleEvaluation = 2

  // @Override
  // public float getEvaluation() {
  // return half;
  // }
  //
  // @Override
  // public boolean revise(RevisionHandler revisator, int reviseCount) {
  // // TODO Auto-generated method stub
  // return false;
  // }

}
