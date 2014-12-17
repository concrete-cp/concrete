package concrete.constraint.semantic;

import scala.annotation.tailrec
import concrete.BooleanDomain
import concrete.Domain
import concrete.FALSE
import concrete.TRUE
import concrete.Variable
import concrete.constraint.Constraint
import concrete.Contradiction
import concrete.ProblemState
import concrete.Outcome

final class Disjunction(scope: Array[Variable],
                        val reverses: IndexedSeq[Boolean]) extends Constraint(scope) {

  require(reverses.size == scope.size, "reverses must cover all variables")

  var watch1: Int = (0 until arity).find(p =>
    scope(p).initDomain.asInstanceOf[BooleanDomain].canBe(!reverses(p))).get
  var watch2: Int = -1

  val ids = scope.map(_.id)

  //if (isTrue(watch1) || isTrue(watch2)) entail()

  def advise(ps: ProblemState, p: Int) = if (p == watch1 || p == watch2) 1 else -1

  def this(scope: Variable*) = this(scope.toArray, new Array[Boolean](scope.size))

  override def check(t: Array[Int]) =
    reverses.zip(t).exists(l => l._1 ^ (l._2 == 1))

  override def toString(ps: ProblemState) =
    "\\/" + (scope, reverses).zipped.map((v, r) => (if (r) "-" else "") + ps.dom(v)).mkString("(", ", ", ")")

  def revise(ps: ProblemState): Outcome = {

    if (isTrue(ps, watch1)) ps.entail(this)
    else {
      val ps0: Outcome =
        if (watch2 < 0 || isFalse(ps, watch2)) {
          seekWatch(ps, watch1) match {
            case None => setTrue(ps, watch1)
            case Some(w) =>
              watch2 = w
              ps
          }
        } else {
          ps
        }

      ps0 andThen {
        ps =>
          if (isTrue(ps, watch2)) ps.entail(this)
          else if (isFalse(ps, watch1)) {

            seekWatch(ps, watch2) match {
              case Some(w) => {
                watch1 = w
                if (isTrue(ps, w)) ps.entail(this)
                else ps
              }
              case None => {
                setTrue(ps, watch2)
              }
            }

          } else {
            ps
          }
      }

    }

  }

  private def isTrue(ps: ProblemState, position: Int) = {
    if (reverses(position)) ps.dom(ids(position)) == FALSE else ps.dom(ids(position)) == TRUE
  }

  private def isFalse(ps: ProblemState, position: Int) =
    if (reverses(position)) ps.dom(ids(position)) == TRUE else ps.dom(ids(position)) == FALSE

  private def setTrue(ps: ProblemState, position: Int): Outcome = {
    if (canBeTrue(ps, position)) {

      if (reverses(position)) {
        ps.updateDom(ids(position), FALSE).entail(this)
      } else {
        ps.updateDom(ids(position), TRUE).entail(this)
      }

    } else {
      Contradiction
    }

  }

  private def canBeTrue(ps: ProblemState, position: Int) = ps.boolDom(ids(position)).canBe(!reverses(position));

  @tailrec
  private def seekWatch(ps: ProblemState, excluding: Int, i: Int = arity - 1): Option[Int] = {
    if (i < 0) None
    else if (i != excluding && canBeTrue(ps, i)) Some(i)
    else seekWatch(ps, excluding, i - 1)
  }

  val simpleEvaluation = 3
}
