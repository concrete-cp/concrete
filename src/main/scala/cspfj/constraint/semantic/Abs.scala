package cspfj.constraint.semantic;

import cspfj.constraint.Constraint
import cspfj.Variable

final class Abs(val result: Variable, val v0: Variable) extends Constraint(Array(result, v0)) {

  //  val corresponding1 = result.dom.allValues map { v0.dom.index }
  //  val corresponding2 = result.dom.allValues map { v => v0.dom.index(-v) }
  //  val correspondingR = v0.dom.allValues map { v => result.dom.index(math.abs(v)) }

  def checkValues(t: Array[Int]) = t(0) == math.abs(t(1))

  private def valid(value: Int, variable: Variable) = {
    val index = variable.dom.index(value)
    index >= 0 && variable.dom.present(index)
  }

  def revise() = {
    var ch = result.dom.intersectVal(v0.dom.valueInterval.abs)

    if (!v0.dom.bound) {
      ch |= result.dom.filter { i =>
        val v = result.dom.value(i)
        v0.dom.presentVal(v) || v0.dom.presentVal(-v)
      }
    }

    ch |= v0.dom.intersectVal(-result.dom.lastValue, result.dom.lastValue)
    ch |= v0.dom.filter { i =>
      result.dom.presentVal(math.abs(v0.dom.value(i)))
    }

    if (ch && result.dom.size == 1 || v0.dom.size == 1) entail()

    ch
  }

  private def revise(v: Variable, f: (Int => Boolean)): Boolean = {
    val change = v.dom.filter(f)
    if (change && v.dom.size == 1) entail()
    change
  }

  override def toString = result + " = |" + v0 + "|";

  def getEvaluation = result.dom.size * 3 / 2 + v0.dom.size

  def simpleEvaluation = 1
}
