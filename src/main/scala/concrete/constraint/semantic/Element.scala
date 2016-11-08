package concrete
package constraint
package semantic

object Element {
  def apply(result: Variable, index: Variable, varsIdx: Seq[(Int, Variable)]) = {
    //val scope = ArrayBuffer(result, index)

    val lastIndex = varsIdx.map(_._1).max
    val vars = Array.ofDim[Variable](lastIndex + 1)

    //    val scopeIndex = Array.fill(lastIndex + 1)(-1)
    for ((i, v) <- varsIdx) {
      vars(i) = v
      //      scopeIndex(i) = scope.size
      //scope += v
    }

    if (vars.forall(v => (v eq null) || v.initDomain.isAssigned)) {

      val values = vars.map(Option(_).map(_.initDomain.singleValue))

      Seq(new ElementVal(result, index, values))
      //      val indices = index.initDomain
      //      val r = result.initDomain
      //
      //      val matrix = new Matrix2D(r.span.size, indices.span.size, r.head, indices.head, false)
      //      for (i <- indices; v <- values(i) if v >= r.head) {
      //        matrix.set(v, i, true)
      //      }
      //      Seq(BinaryExt(Array(result, index), matrix))
    } else {
      Seq(
        new Element(result, index, vars))
    }
  }
}

class ElementVal(val result: Variable, val index: Variable, val valuesOpt: Array[Option[Int]]) extends Constraint(Array(result, index)) {

  def advise(ps: ProblemState, event: Event, position: Int): Int = ps.card(result) + ps.card(index)

  var values: Array[Int] = _

  // val offset = valuesOpt.flatten.min

  def check(tuple: Array[Int]): Boolean = {
    // println(s"${valuesOpt(tuple(1))} = ${tuple(0)} ?")

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
    //println(s"${tuple.toSeq} ${tuple(2 + tuple(1))}")
    tuple(1) < vars.length && (vars(tuple(1)) ne null) && (tuple(0) == tuple(map(tuple(1))))
    //map.get(tuple(1)).forall(i => tuple(0) == tuple(i))
    //tuple(1) < arity - 2 && tuple(0) == tuple(map(tuple(1)))
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
//    if (result.name == "X_INTRODUCED_1999") {
//      println(toString(ps))
//    }
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
//      .andThen { r =>
//        if (result.name == "X_INTRODUCED_1999") {
//
//          println(s"${ps.dom(result)} ${ps.dom(index)} ${vars.map(ps.dom).mkString("{", ", ", "}")}")
//          println("---")
//        }
//        r
//      }

  }
  def simpleEvaluation: Int = 3

  override def toString(ps: ProblemState) = toString(ps, "AC")
}