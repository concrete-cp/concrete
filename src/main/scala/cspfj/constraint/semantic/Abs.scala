package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint
import cspfj.problem.Variable

final class Abs(val result: Variable, val v0: Variable) extends AbstractConstraint(Array(result, v0)) {

  //  val corresponding1 = result.dom.allValues map { v0.dom.index }
  //  val corresponding2 = result.dom.allValues map { v => v0.dom.index(-v) }
  //  val correspondingR = v0.dom.allValues map { v => result.dom.index(math.abs(v)) }

  def check = value(0) == math.abs(value(1))

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

    ch |= v0.dom.removeFromVal(result.dom.lastValue + 1)
    ch |= v0.dom.removeToVal(-result.dom.lastValue - 1)
    ch |= v0.dom.filter { i =>
      val v = v0.dom.value(i)
      result.dom.presentVal(math.abs(v))
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
