package cspfj.constraint.semantic;

import java.util.Arrays
import cspfj.constraint.AbstractConstraint
import cspfj.problem.BooleanDomain
import cspfj.problem.Variable
import cspfj.UNSATException
import scala.annotation.tailrec

final class Disjunction(scope: Array[Variable],
  val reverses: IndexedSeq[Boolean]) extends AbstractConstraint(scope) {

  require(scope forall (v => v.dom.isInstanceOf[BooleanDomain] && v.dom.size == 2),
    "Only non-constant boolean domains are allowed")
  require(reverses != null)
  require(reverses.size == scope.size, "reverses must cover all variables")

  val domains = scope map (_.dom.asInstanceOf[BooleanDomain])

  var watch1 = seekWatch(-1).get

  var watch2 = seekWatch(watch1) match {
    case Some(w) => w
    case None => {
      setTrue(watch1)
      entail()
      watch1
    }
  }

  val getEvaluation = Integer.highestOneBit(arity)

  def this(scope: Variable*) = this(scope.toArray, new Array[Boolean](scope.size))

  override def checkIndices(t: Array[Int]) = reverses.zip(t).exists(l => l._1 ^ l._2 == 1)
  
  def checkValues(t: Array[Int]) = checkIndices(t)

  override def toString = "\\/" + scope.mkString("(", ", ", ")")

  def revise(): Boolean = {
    if (isTrue(watch1) || isTrue(watch2)) entail()
    else {
      if (isFalse(watch1)) {

        seekWatch(watch2) match {
          case Some(w) => {
            watch1 = w
            if (isTrue(w)) {
              entail()
              return false
            }
          }
          case None => {
            setTrue(watch2)
            entail()
            return true
          }
        }

      }

      if (isFalse(watch2)) {

        seekWatch(watch1) match {
          case Some(w) => {
            watch2 = w
            if (isTrue(w)) entail()
          }
          case None => {
            setTrue(watch1)
            entail()
            return true
          }
        }

      }
    }
    false
  }

  private def isTrue(position: Int) = {
    if (reverses(position)) domains(position).isFalse else domains(position).isTrue
  }

  private def isFalse(position: Int) =
    if (reverses(position)) domains(position).isTrue else domains(position).isFalse

  private def setTrue(position: Int) {
    if (canBeTrue(position)) {

      if (domains(position).isUnknown) {

        if (reverses(position)) domains(position).setFalse()
        else domains(position).setTrue()

      }
    } else throw UNSATException.e

    // } else sys.error("Unreachable state: " + dom + " / " + reverses(position))

  }

  private def canBeTrue(position: Int) = domains(position).canBe(!reverses(position));

  private def seekWatch(excluding: Int) = {
    @tailrec
    def find(i: Int): Option[Int] =
      if (i < 0) None
      else if (i != excluding && canBeTrue(i)) Some(i)
      else find(i - 1)

    find(arity - 1)
  }
  val simpleEvaluation = 3
}
