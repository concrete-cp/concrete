/*
 * Created on 19 mars 2007
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint.extension;

import java.util.Arrays;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;

final class TupleManager(val constraint: Constraint, val tuple: Array[Int]) {

  private val arity = constraint.arity

  def setFirstTuple() {
    for (p <- tuple.indices) {
      tuple(p) = constraint.scope(p).dom.first
    }
  }

  private def setNextTupleR(pos: Int): Boolean = {
    if (pos < 0) false else {
      val index = constraint.scope(pos).dom.next(tuple(pos))
      if (index < 0) {
        tuple(pos) = constraint.scope(pos).dom.first
        setNextTupleR(pos - 1)
      } else {
        tuple(pos) = index
        true
      }
    }
  }

  def setNextTuple() = setNextTupleR(arity - 1)

  def setFirstTuple(variablePosition: Int, index: Int) {
    for (p <- tuple.indices) {
      if (p == variablePosition) {
        tuple(p) = index
      } else {
        tuple(p) = constraint.scope(p).dom.first
      }
    }
  }

  private def setNextTupleR(fixedVariablePosition: Int, pos: Int): Boolean = {
    if (pos < 0) {
      false
    } else if (pos == fixedVariablePosition) {
      setNextTupleR(fixedVariablePosition, pos - 1)
    } else {
      val index = constraint.scope(pos).dom.next(tuple(pos))

      if (index < 0) {
        tuple(pos) = constraint.scope(pos).dom.first
        setNextTupleR(fixedVariablePosition, pos - 1)
      } else {
        tuple(pos) = index
        true
      }
    }
  }

  def setNextTuple(fixedVariablePosition: Int) = setNextTupleR(fixedVariablePosition, arity - 1)

  def setFirstTuple(base: Array[Int]) {
    for (p <- tuple.indices) {
      if (base(p) >= 0) {
        tuple(p) = base(p)
      } else {
        tuple(p) = 0
      }
    }
  }

  private def setNextTupleR(base: Array[Int], p: Int): Boolean = {
    if (p < 0) {
      false
    } else if (base(p) >= 0) {
      setNextTupleR(base, p - 1)
    } else {
      val index = tuple(p) + 1
      if (index >= constraint.scope(p).dom.maxSize) {
        tuple(p) = 0
        setNextTupleR(base, p - 1)
      } else {
        tuple(p) = index
        true
      }
    }
  }

  def setNextTuple(base: Array[Int]) = setNextTupleR(base, arity - 1)

  def setTuple(tpl: Array[Int]) {
    tpl.copyToArray(tuple)
    assert(allPresent)
  }

  private def allPresent =
    (tuple, constraint.scope).zipped.forall((i, v) => v.dom.present(i))

  def setTupleAfter(tpl: Array[Int], fixed: Int): Boolean = {
    tpl.copyToArray(tuple)
    var changed = arity;
    var pos = 0;
    while (pos < arity) {
      if (pos == fixed) {
        pos += 1;
      } else {

        val variable = constraint.scope(pos)

        if (pos > changed) {
          tuple(pos) = variable.dom.first;
          pos += 1
        } else {

          var index = tuple(pos)

          if (variable.dom.present(index)) {
            pos += 1
          } else {
            changed = pos;
            do {
              index = variable.dom.next(index);
            } while (index >= 0 && !variable.dom.present(index));

            if (index < 0) {
              do {
                do {
                  pos -= 1;
                } while (pos == fixed);
                if (pos < 0) {
                  return false;
                }
                tuple(pos) = constraint.scope(pos).dom.next(
                  tuple(pos));
              } while (tuple(pos) < 0);
              changed = pos;
            } else {
              tuple(pos) = index;
              pos += 1
            }
          }
        }
      }
    }

    assert(allPresent, tpl.iterator + " -> "
      + tuple.iterator)

    return true;
  }

  private def setPrevTupleR(fVP: Int, p: Int): Boolean = {
    if (p < 0) {
      false
    } else if (p == fVP) {
      setPrevTupleR(fVP, p - 1)
    } else {
      val index = constraint.scope(p).dom.prev(tuple(p))
      if (index < 0) {
        tuple(p) = constraint.scope(p).dom.last
        setPrevTupleR(fVP, p - 1)
      } else {
        tuple(p) = index
        true
      }
    }
  }

  def setPrevTuple(fixedVariablePosition: Int) = setPrevTupleR(fixedVariablePosition, arity - 1)

}
