package concrete
package constraint
package semantic

import cspom.util.BitVector
import scala.collection.mutable.ArrayBuffer

object Circuit {
  def apply(scope: (Int, Variable)*): Circuit = apply(scope.toMap)
  def apply(scope: Map[Int, Variable]): Circuit = {
    val offset = scope.keys.min
    val max = scope.keys.max - offset

    val array: Array[Variable] = Array.fill(scope.keys.max - offset + 1)(null)

    val constraintScope = new ArrayBuffer[Variable]
    val vars2scope: Array[Int] = Array.fill(scope.keys.max - offset + 1)(-1)
    val scope2vars = new ArrayBuffer[Int]

    for ((i, v) <- scope) {
      array(i - offset) = v
      vars2scope(i - offset) = constraintScope.size
      scope2vars += i - offset
      constraintScope += v
    }

    new Circuit(offset, array, constraintScope.toArray,
      vars2scope, scope2vars.toArray)

  }
}

class Circuit(
  private val offset: Int,
  private val hScope: Array[Variable],
  constraintScope: Array[Variable],
  private val vars2scope: Array[Int],
  private val scope2vars: Array[Int])
    extends Constraint(constraintScope) {

  private val startWith = scope2vars(0)

  def advise(problemState: concrete.ProblemState, pos: Int): Int = arity
  def check(tuple: Array[Int]): Boolean = {
    check(tuple, scope2vars(0), BitVector.empty, arity)
  }

  private def check(tuple: Array[Int], current: Int, visited: BitVector, remaining: Int): Boolean = {

    if (remaining == 0) {
      current == startWith
    } else if (visited(current)) {
      false
    } else {
      val si = vars2scope(current)
      val next = tuple(si) - offset

      check(tuple,
        next,
        visited + current,
        remaining - 1)
    }
  }

  def init(ps: concrete.ProblemState): Outcome = ps
  def revise(ps: concrete.ProblemState): Outcome = {
    //println(toString(ps))
    var N = 0
    var L: List[Int] = Nil
    val dfsnum = new Array[Int](hScope.length + 1)
    val low = new Array[Int](hScope.length + 1)
    var T = BitVector.empty
    var onepartfound = false

    def visit(p: Int): Boolean = {
      L ::= p
      dfsnum(p) = N
      N += 1
      low(p) = dfsnum(p)
      for (oq <- ps.dom(hScope(p))) {
        val q = oq - offset
        if (T(q)) {
          low(p) = math.min(low(p), dfsnum(q))
        } else {
          T += q
          if (!visit(q)) return false
          low(p) = math.min(low(p), low(q))
        }
      }

      if (low(p) == dfsnum(p)) {
        if (onepartfound) {
          //println("second component found")
          false
        } else {
          onepartfound = true
          L = L.dropWhile { v => v != p }
          //          var v = 0
          //          //println("component")
          //          do {
          //            v = L.head
          //            //println(v)
          //            L = L.tail
          //          } while (v != p)
          true
        }
      } else {
        true
      }

    }

    if (visit(startWith) && T.cardinality == arity) {
      ps
    } else {
      Contradiction
    }

  }

  def simpleEvaluation: Int = 3
}