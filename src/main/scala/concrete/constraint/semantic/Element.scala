package concrete
package constraint
package semantic

import scala.collection.mutable.HashMap
import java.util.Arrays
import cspom.util.BitVector

object Element {
  def apply(result: Variable, index: Variable, varsIdx: Seq[(Int, Variable)]) = {
    val lastIndex = varsIdx.map(_._1).max
    val vars = Array.ofDim[Variable](lastIndex + 1)

    for ((i, v) <- varsIdx) {
      vars(i) = v
    }

    if (vars.forall(v => (v eq null) || v.initDomain.isAssigned)) {
      val values = vars.map(Option(_).map(_.initDomain.singleValue))
      Seq(new ElementVal(result, index, values))
    } else {
      Seq(new ElementWatch(result, index, vars))
    }
  }
}

class ElementVal(val result: Variable, val index: Variable, val valuesOpt: Array[Option[Int]]) extends Constraint(Array(result, index)) {

  def advise(ps: ProblemState, event: Event, position: Int): Int = ps.card(result) + ps.card(index)

  var values: Array[Int] = _

  def check(tuple: Array[Int]): Boolean = {
    valuesOpt(tuple(1)).contains(tuple(0))
  }

  def init(ps: ProblemState): Outcome = {
    values = valuesOpt.map {
      case Some(v) => v
      case None => Int.MinValue
    }

    ps.filterDom(index)(valuesOpt(_).isDefined)
  }

  def revise(ps: ProblemState): Outcome = {
    val res = ps.dom(result)
    val ind = ps.dom(index)
    ps.updateDom(index, ind.filter(i => res.present(values(i))))
      .andThen { ps =>
        var bv: IntDomain = EmptyIntDomain
        for (i <- ps.dom(index)) {
          bv |= values(i)
        }
        ps.intersectDom(result, bv)
      }
      .entailIf(this, _.dom(result).isAssigned)
  }

  def simpleEvaluation: Int = ???
}

class ElementWatch(val result: Variable,
  val index: Variable,
  val vars: Array[Variable])
    extends Constraint(result +: index +: vars.filter(_ ne null)) with Removals {

  private var card: Int = _

  /**
   * For each value v in result, contains an index i for which vars(i) may contain v
   */
  private val resultWatches = new HashMap[Int, Int]

  /**
   * For each index i, contains a value "v" for which result and vars(i) may contain v
   */
  private val indexWatches = new Array[Int](vars.length)

  /**
   * For each variable in vars, counts the number of current resultWatches
   */
  private val watched = Array.fill(vars.length)(0)

  private val pos2vars = {
    val varsIndices = vars.zipWithIndex.toMap
    Array.tabulate(arity)(i => varsIndices.getOrElse(scope(i), -1))
  }

  private lazy val vars2pos = {
    val scopeIndices = scope.zipWithIndex.drop(2).toMap
    Array.tabulate(vars.length)(i => if (vars(i) eq null) -1 else scopeIndices(vars(i)))

  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(1) < vars.length &&
      (vars(tuple(1)) ne null) &&
      (tuple(0) == tuple(vars2pos(tuple(1))))
  }

  def toString(ps: ProblemState, consistency: String): String = {
    s"${result.toString(ps)} =$consistency= (${index.toString(ps)})th of ${
      vars.toSeq.map {
        case null => "{}"
        case v => v.toString(ps)
      }
    }"
  }

  def getEvaluation(ps: ProblemState): Int = {
    card * ps.card(index)
  }

  override def init(ps: ProblemState) = {
    val notnull = ps.dom(index).filter(i => i >= 0 && i < vars.length && (vars(i) ne null))
    card = notnull.view.map(i => ps.card(vars(i))).max
    ps.updateDom(index, notnull)
  }

  private def reviseIndex(ps: ProblemState, resultDom: Domain): Outcome = {
    ps.filterDom(this.index) { i =>
      val dom = ps.dom(vars(i))
      val v = indexWatches(i)
      (resultDom.present(v) && dom.present(v)) || {
        val support = (resultDom & dom).headOption
        support.foreach(s => indexWatches(i) = s)
        support.isDefined
      }
    }
  }

  private def reviseAssignedIndex(ps: ProblemState, index: Int, resultDom: Domain): Outcome = {
    val selectedVar = vars(index)
    //println(selectedVar.toString(ps))
    val intersect = ps.dom(selectedVar) & resultDom

    //println(s"${ps.dom(selectedVar)} & $resultDom = $intersect")

    ps
      .updateDom(selectedVar, intersect)
      .andThen { ps =>
        if (intersect.size < resultDom.size) {
          ps.updateDom(result, intersect)
        } else {
          ps
        }
      }
  }

  private def reviseResult(ps: ProblemState, index: Domain): Outcome = {
    ps.filterDom(result) { i =>
      val oldWatch = resultWatches.get(i)
      oldWatch.exists(v => index.present(v) && ps.dom(vars(v)).present(i)) || {
        val support = index.find(j => ps.dom(vars(j)).present(i))
        support.foreach { s =>
          oldWatch.foreach(v => watched(v) -= 1)
          resultWatches(i) = s
          watched(s) += 1
        }
        support.isDefined
      }

    }
  }

  private def invalidResultWatch(ps: ProblemState, mod: BitVector): Boolean = {
    var m = mod.nextSetBit(2)
    while (m >= 0) {
      val i = pos2vars(m)
      if (watched(i) > 0) {
        return true
      }
      m = mod.nextSetBit(m + 1)
    }
    return false
  }

  private def invalidIndexWatch(ps: ProblemState, mod: BitVector, resultDom: Domain): Boolean = {
    var m = mod.nextSetBit(2)
    while (m >= 0) {
      val i = pos2vars(m)
      if (!resultDom.present(indexWatches(i)) || !ps.dom(vars(i)).present(indexWatches(i))) {
        return true
      }
      m = mod.nextSetBit(m + 1)
    }
    return false
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {

    var m = mod.nextSetBit(0)
    require(m >= 0)
    var rResult = false
    var rIndex = false

    val resultDom = ps.dom(result)

    if (m >= 2) {
      // result and index are not modified
      rResult = invalidResultWatch(ps, mod)
      rIndex = invalidIndexWatch(ps, mod, resultDom)
    } else if (m == 1) {
      // index is modified, result is not modified
      rResult = true
      rIndex = invalidIndexWatch(ps, mod, resultDom)
    } else {
      // result is modified
      rIndex = true
      rResult = (mod.nextSetBit(1) == 1) || invalidResultWatch(ps, mod)
    }

    (if (rIndex) {
      reviseIndex(ps, resultDom)
    } else {
      ps
    }).andThen { ps =>

      val index = ps.dom(this.index)

      if (index.isAssigned) {
        reviseAssignedIndex(ps, index.singleValue, resultDom)
      } else if (rResult) {
        reviseResult(ps, index)
      } else {
        ps
      }
    }
  }

  def simpleEvaluation: Int = 3

  override def toString(ps: ProblemState) = toString(ps, "AC")
}

class Element(val result: Variable,
  val index: Variable,
  val vars: Array[Variable])
    extends Constraint(result +: index +: vars.filter(_ ne null)) {

  private var card: Int = _

  lazy val map: Map[Int, Int] = {
    val scopeIndices = scope.zipWithIndex.toMap
    vars.indices.flatMap(i =>
      Option(vars(i)).map(i -> scopeIndices(_))).toMap
  }

  def check(tuple: Array[Int]): Boolean = {
    tuple(1) < vars.length &&
      (vars(tuple(1)) ne null) &&
      (tuple(0) == tuple(map(tuple(1))))
  }

  def toString(ps: ProblemState, consistency: String): String = {
    s"${result.toString(ps)} =$consistency= (${index.toString(ps)})th of ${
      vars.toSeq.map {
        case null => "{}"
        case v => v.toString(ps)
      }
    }"
  }

  def advise(ps: ProblemState, event: Event, pos: Int): Int = {
    card * ps.card(index)
  }

  override def init(ps: ProblemState) = {
    val notnull = ps.dom(index).filter(i => i >= 0 && i < vars.length && (vars(i) ne null))
    card = notnull.view.map(i => ps.card(vars(i))).max
    ps.updateDom(index, notnull)
  }

  def revise(ps: ProblemState): Outcome = {
    val resultDom = ps.dom(result)
    /*
     * Revise indices
     */
    ps.filterDom(this.index) { i =>
      !resultDom.disjoint(ps.dom(vars(i)))
    }
      .andThen { ps =>
        val index = ps.dom(this.index)

        if (index.isAssigned) {
          val selectedVar = vars(index.singleValue)
          //println(selectedVar.toString(ps))
          val intersect = ps.dom(selectedVar) & resultDom

          //println(s"${ps.dom(selectedVar)} & $resultDom = $intersect")

          ps
            .updateDom(selectedVar, intersect)
            .andThen { ps =>
              if (intersect.size < resultDom.size) {
                ps.updateDom(result, intersect)
              } else {
                ps
              }
            }
        } else {

          /*
           * Revise result
           * Do not use map/reduce, it is too slow
           */
          var union: Domain = null
          for (i <- index) {
            if (null == union) union = ps.dom(vars(i))
            else union |= ps.dom(vars(i))
          }

          ps.updateDom(result, resultDom & union)

        }
      }
  }

  def simpleEvaluation: Int = 3

  override def toString(ps: ProblemState) = toString(ps, "AC")
}