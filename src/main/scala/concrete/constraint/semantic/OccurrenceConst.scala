package concrete.constraint.semantic

import concrete.Variable
import concrete.constraint.Constraint
import concrete.constraint.Residues
import concrete.constraint.TupleEnumerator
import concrete.UNSATObject

class OccurrenceConst(val result: Int, val value: Int, val vars: Array[Variable])
  extends Constraint(vars) {

  def checkValues(tuple: Array[Int]) =
    result == (0 until arity).count(i => tuple(i) == value)

  def advise(pos: Int): Int = arity

  def revise(): Traversable[Int] = {
    var affected = 0
    var canBeAffected = 0

    for (v <- vars) {
      if (v.dom.presentVal(value)) {
        if (v.dom.size == 1) {
          affected += 1
        } else {
          canBeAffected += 1
        }
      }
    }

    if (affected + canBeAffected < result || affected > result) {
      throw UNSATObject
    }
    if (affected == result && canBeAffected > 0) {
      (0 until arity).filter(v => scope(v).dom.size > 1 && scope(v).dom.removeVal(value)) //.toList
    } else if (affected + canBeAffected == result) {
      (0 until arity).filter(
        p =>
          if (scope(p).dom.size > 1 && scope(p).dom.present(value)) {
            scope(p).dom.setSingle(value)
            true
          } else {
            false
          }) //.toList
    } else {
      Nil
    }

  }

  def simpleEvaluation: Int = ???

}
