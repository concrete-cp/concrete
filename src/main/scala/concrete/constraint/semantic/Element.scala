package concrete
package constraint
package semantic

import bitvectors.BitVector
import concrete.util.Interval

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Element {
  def apply(result: Variable, index: Variable, varsIdx: Seq[(Int, Variable)]): Seq[Constraint] = {
    val lastIndex = varsIdx.map(_._1).max
    val vars = Array.ofDim[Variable](lastIndex + 1)

    for ((i, v) <- varsIdx) {
      vars(i) = v
    }

    if (result eq index) {
      Seq(new ElementRI(result, vars))
    } else if (vars.forall(v => (v eq null) || v.initDomain.isAssigned)) {
      val values = vars.map(Option(_).map(_.initDomain.singleValue))
      Seq(new ElementVal(result, index, values))
    } else {
      Seq(
        //new ElementBC(result, index, vars),
        new ElementWatch(result, index, vars))
    }
  }


}

trait Element extends Constraint {
  private lazy val vars2pos = {
    val scopeIndices = scope.zipWithIndex.drop(2).toMap
    Array.tabulate(vars.length)(i => Option(vars(i)).map(scopeIndices).getOrElse(-1))
  }

  def vars: Array[Variable]

  def index: Variable

  def result: Variable

  override def init(ps: ProblemState): Outcome = {
    ps.filterDom(index)(i => i >= 0 && i < vars.length && (vars(i) ne null))
  }


  def check(tuple: Array[Int]): Boolean = {
    tuple(1) < vars.length &&
      (vars(tuple(1)) ne null) &&
      (tuple(0) == tuple(vars2pos(tuple(1))))
  }

  protected def reviseAssignedIndex(ps: ProblemState, index: Int, resultDom: Domain): Outcome = {
    val selectedVar = vars(index)
    //println(selectedVar.toString(ps))
    val intersect = ps.dom(selectedVar) & resultDom

    // println(s"${ps.dom(selectedVar)} & $resultDom = $intersect")

    ps
      .updateDom(selectedVar, intersect)
      .updateDom(result, intersect)
      .entailIf(this, _ => intersect.isAssigned)
  }
}

/**
  * Special case of Element to be used when the array contains only constants
  *
  * @param result    : result variable
  * @param index     : index variables
  * @param valuesOpt : array of optional values
  */
class ElementVal(val result: Variable, val index: Variable, val valuesOpt: Array[Option[Int]])
  extends Constraint(Array(result, index)) {

  private val offset = valuesOpt.flatten.min
  var values: Array[Int] = _
  var indices: Array[Seq[Int]] = _

  def advise(ps: ProblemState, event: Event, position: Int): Int = ps.card(result) + ps.card(index)

  def check(tuple: Array[Int]): Boolean = {
    valuesOpt(tuple(1)).contains(tuple(0))
  }

  def init(ps: ProblemState): Outcome = {
    values = valuesOpt.map {
      case Some(v) => v
      case None => Int.MinValue
    }

    val allIndices = new mutable.HashMap[Int, mutable.Seq[Int]]().withDefaultValue(new ArrayBuffer())

    for (i <- ps.dom(index); value <- valuesOpt(i)) {
      allIndices(value) :+= i
    }

    indices = Array.tabulate(allIndices.keysIterator.max - offset + 1)(i => allIndices(i + offset).toArray)


    ps.filterDom(index)(valuesOpt(_).isDefined)
      .filterDom(result)(allIndices(_).nonEmpty)
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val res = ps.dom(result)

    ps.filterDom(index)(i => res.present(values(i)))
      //      .andThen { ps =>
      //        val iDom = ps.dom(index).view.map(values)
      //        val offset = iDom.min
      //        val bv = new java.util.BitSet()
      //        for (i <- iDom) {
      //          bv.set(i - offset)
      //        }
      //        ps.filterDom(result)(v => v >= offset && bv.get(v - offset))
      //      }
      .andThen { ps =>
      val iDom = ps.dom(index)
      ps.filterDom(result)(v => indices(v - offset).exists(iDom.present))
    }
      .entailIf(this, _.dom(index).isAssigned)
  }

  def simpleEvaluation: Int = ???
}

/**
  * Special case of Element to be used when result = index
  *
  * @param resultIndex : result/index variable
  * @param vars        : array variables
  */
class ElementRI(val resultIndex: Variable, val vars: Array[Variable]) extends Constraint(resultIndex +: vars.flatMap(Option(_))) {
  private lazy val vars2pos = {
    val scopeIndices = scope.zipWithIndex.drop(1).toMap
    Array.tabulate(vars.length)(i => Option(vars(i)).map(scopeIndices).getOrElse(-1))
  }

  def advise(problemState: ProblemState, event: Event, position: Int): Int = arity

  def check(tuple: Array[Int]): Boolean = {
    tuple(0) < vars.length &&
      (vars(tuple(0)) ne null) &&
      (tuple(0) == tuple(vars2pos(tuple(0))))
  }

  def init(ps: ProblemState): Outcome = ps

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    ps.filterDom(resultIndex) { i =>
      ps.dom(vars(i)).present(i)
    }.andThen { ps =>
      if (ps.dom(resultIndex).isAssigned) {
        val value = ps.dom(resultIndex).singleValue
        ps.tryAssign(vars(value), value).entail(this)
      } else {
        ps
      }
    }
  }

  def simpleEvaluation: Int = 2
}

/**
  * Standard bound-consistency Element constraint
  */
class ElementBC(val result: Variable, val index: Variable, val vars: Array[Variable])
  extends Constraint(result +: index +: vars.filter(_ ne null)) with Element with BC {

  def advise(ps: ProblemState, pos: Int): Int = 4 * ps.card(index)

  def shave(ps: ProblemState): Outcome = {
    val rDom = ps.dom(result)

    var union: Interval = null

    ps.filterDom(index) { i =>
      val dom = ps.dom(vars(i))
      if (union == null) union = dom.span else union = union.span(dom.span)
      !rDom.disjoint(dom)
    }
      .andThen { ps =>
        val iDom = ps.dom(this.index)
        if (iDom.isAssigned) {
          reviseAssignedIndex(ps, iDom.singleValue, rDom)
        } else {
          ps.shaveDom(result, union)
        }
      }

  }

  def simpleEvaluation: Int = 2
}

/**
  * Standard Element constraint implemented with watches
  *
  * @param result : result variable
  * @param index  : index variable
  * @param vars   : array variables
  */
class ElementWatch(val result: Variable,
                   val index: Variable,
                   val vars: Array[Variable])
  extends Constraint(result +: index +: vars.filter(_ ne null)) with Element {

  require(result ne index)


  private val pos2vars = Array.fill(arity)(-1) //new Array[Int](arity)

  fillPos2Vars(2, 0)

  /**
    * resultWatches(p) contains values that support some result value (ie, p is a valid index
    * and all resultWatches(p) are present in vars(p))
    */
  private[semantic] val resultWatches = Array.fill(vars.length)(List[Int]())

  /**
    * indexWatches(i) contains the value v that support index i (ie, result and vars(i) both contains v)
    */
  private[semantic] val indexWatches = mutable.Map[Int, Int]()

  private var card: Int = _

  def advise(ps: ProblemState, event: Event, pos: Int): Int = {
    card * ps.card(index)
  }

  override def init(ps: ProblemState): Outcome = {
    super[Element].init(ps).andThen { ps =>

      val rDom = ps.dom(result)
      ps.filterDom(index) { i =>
        // Initializes index watches
        findIndexWatch(ps, i, rDom) match {
          case Some(v) =>
            indexWatches(i) = v
            true
          case None => false
        }
      }
        .andThen { ps =>
          val iDom = ps.dom(index)
          // Computes max cardinality
          card = iDom.view.map(i => ps.card(vars(i))).max

          // Initializes result watches
          ps.filterDom(result) { v =>
            findResultWatch(ps, v, iDom) match {
              case Some(i) =>
                resultWatches(i) ::= v
                true
              case None => false
            }
          }
        }
    }
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    var rDom = ps.dom(result)
    (if (mod(0)) {
      // Result is modified, all indices must be rechecked :(
      ps.filterDom(index) { i => checkIndexWatches(ps, i, rDom) }
    } else {
      // Result is not modified, only check indices that have been touched
      var iDom = ps.dom(index)
      var m = mod.nextSetBit(2)
      while (m >= 0) {
        val p = pos2vars(m)
        if (iDom.present(p) && !checkIndexWatches(ps, p, rDom)) {
          iDom = iDom.remove(p)
        }

        m = mod.nextSetBit(m + 1)
      }
      ps.updateDom(index, iDom)
    })
      .andThen { ps =>
        val iDom = ps.dom(index)

        // Then check if index is assigned (result = vars(i) in this case)
        if (iDom.isAssigned) {
          reviseAssignedIndex(ps, iDom.singleValue, rDom)
        } else {
          if (mod(1)) {
            // Indices have been modified
            for (i <- resultWatches.indices if !iDom.present(i)) {
              for (w <- resultWatches(i)) {
                // Watches must be recomputed
                findResultWatch(ps, w, iDom) match {
                  case Some(p) => resultWatches(p) ::= w
                  case None => rDom.removeIfPresent(w)
                }

              }
              resultWatches(i) = Nil
            }

          } else {
            // Finally, filter result
            var m = mod.nextSetBit(2)
            while (m >= 0) {
              val p = pos2vars(m)
              rDom = checkResultWatches(ps, p, rDom, iDom)

              m = mod.nextSetBit(m + 1)
            }
          }
          ps.updateDom(result, rDom)

        }
      }
  }

  private def checkResultWatches(ps: ProblemState, mod: Int, rDom: Domain, iDom: Domain): Domain = {
    var filtered = rDom
    val dom = ps.dom(vars(mod))


    resultWatches(mod) = resultWatches(mod).filter { w =>
      (dom.present(w) && iDom.present(mod)) || {
        // Watch is invalid, seek another one
        findResultWatch(ps, w, iDom) match {
          case Some(i) =>
            require(i != mod)
            resultWatches(i) ::= w

          case None =>
            filtered = filtered.removeIfPresent(w)
        }
        false
      }
    }

    filtered
  }

  private def findResultWatch(ps: ProblemState, v: Int, indices: Domain) = indices.find(i => ps.dom(vars(i)).present(v))

  private def checkIndexWatches(ps: ProblemState, i: Int, rDom: Domain): Boolean = {
    val watch = indexWatches(i)
    if (ps.dom(vars(i)).present(watch) && rDom.present(watch)) {
      true
    } else {
      findIndexWatch(ps, i, rDom) match {
        case Some(w) =>
          indexWatches(i) = w
          true
        case None =>
          false
      }
    }
  }

  private def findIndexWatch(ps: ProblemState, i: Int, result: Domain) = (result & ps.dom(vars(i))).headOption

  def simpleEvaluation: Int = 3

  override def toString(ps: ProblemState): String = toString(ps, "AC")

  def toString(ps: ProblemState, consistency: String): String = {
    s"${result.toString(ps)} =$consistency= ${index.toString(ps)}th of [${
      vars.map(Option(_).map(_.toString(ps)).getOrElse("{}")).mkString(", ")
    }]"
  }

  private def fillPos2Vars(scopeI: Int, varsI: Int): Unit = {
    if (scopeI < arity) {
      Option(vars(varsI)) match {
        case None => fillPos2Vars(scopeI, varsI + 1)
        case Some(v) =>
          assert(scope(scopeI) == v)
          pos2vars(scopeI) = varsI
          fillPos2Vars(scopeI + 1, varsI + 1)
      }
    }
  }
}
