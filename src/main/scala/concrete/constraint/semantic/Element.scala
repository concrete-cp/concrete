package concrete
package constraint
package semantic

import bitvectors.BitVector
import concrete.util.IntIntMap

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
      Seq(new ElementWatch(result, index, vars))
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
  * Special case of Element to be used when the array contains only constants.
  * Special case of binary constraint: indices have exactly one support
  *
  * @param result    : result variable
  * @param index     : index variables
  * @param valuesOpt : array of optional values
  */
class ElementVal(val result: Variable, val index: Variable, val valuesOpt: Array[Option[Int]])
  extends Constraint(Array(result, index)) {

  private val offset = valuesOpt.flatten.min
  var values: Array[Int] = _
  var indices: Array[Array[Int]] = _

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

    ps.filterDom(index)(i => res.contains(values(i)))
      .andThen { ps =>
        val iDom = ps.dom(index)
        ps.filterDom(result)(v => indices(v - offset).exists(iDom))
      }
      .entailIf(this, _.dom(index).isAssigned)
  }

  def simpleEvaluation: Int = 1
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
      ps.dom(vars(i)).contains(i)
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
    * indexWatches(i) contains the value v that support index i (ie, result and vars(i) both contains v)
    */
  private[semantic] val indexResidues = new IntIntMap(index.initDomain.size)
  private val card: Int = vars.filter(_ ne null).map(_.initDomain.size).max
  /**
    * resultWatches(v) contains an index i that support value v (ie, i is a valid index
    * and vars(i) contains v)
    */
  private[semantic] var resultResidues = new IntIntMap(result.initDomain.size)

  def advise(ps: ProblemState, event: Event, pos: Int): Int = {
    card * ps.card(index)
  }

  def revise(ps: ProblemState, mod: BitVector): Outcome = {
    val rDom = ps.dom(result)

    val afterIndex = if (mod(1) && mod.size == 1) {
      // Index revision is skipped if it is the only one modified
      ps
    } else {
      ps.filterDom(index) { i =>
        val validResidue = indexResidues.get(i)
          .exists(v => rDom.contains(v) && ps.dom(vars(i)).contains(v))

        validResidue || {
          val support = (rDom & ps.dom(vars(i))).headOption
          for (v <- support) indexResidues.justPut(i, v)
          support.isDefined
        }
      }
    }

    afterIndex.andThen { ps =>
      val iDom = ps.dom(index)
      if (iDom.isAssigned) {
        reviseAssignedIndex(ps, iDom.head, rDom)
      } else {
        ps.filterDom(result) { v =>
          val validResidue = resultResidues.get(v)
            .exists(i => iDom.contains(i) && ps.dom(vars(i)).contains(v))

          validResidue || {
            val support = iDom.find(i => ps.dom(vars(i)).contains(v))
            for (i <- support) resultResidues.justPut(v, i)
            support.nonEmpty
          }
        }
      }
    }

  }


  override def toString(ps: ProblemState): String = toString(ps, "AC")

  def toString(ps: ProblemState, consistency: String): String = {
    s"${result.toString(ps)} =$consistency= ${index.toString(ps)}th of [${
      vars.map(Option(_).map(_.toString(ps)).getOrElse("{}")).mkString("\n")
    }]"
  }

  def simpleEvaluation: Int = 3

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
