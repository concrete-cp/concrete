package concrete.constraint.semantic;

import java.util.Arrays
import concrete.constraint.Constraint
import concrete.BooleanDomain
import concrete.Variable
import concrete.UNSATException
import scala.annotation.tailrec
import concrete.UNSATObject

final class Disjunction(scope: Array[Variable],
  val reverses: IndexedSeq[Boolean]) extends Constraint(scope) {

  require(reverses.size == scope.size, "reverses must cover all variables")

  val domains = scope map (_.dom.asInstanceOf[BooleanDomain])

  var watch1 = seekWatch(-1).get

  var watch2 = seekWatch(watch1) match {
    case Some(w) => w
    case None => {
      setTrue(watch1)
      watch1
    }
  }

  if (isTrue(watch1) || isTrue(watch2)) entail()

  private val eval = Integer.highestOneBit(arity) - 1

  def advise(p: Int) = if (p == watch1 || p == watch2) eval else -1

  def this(scope: Variable*) = this(scope.toArray, new Array[Boolean](scope.size))

  override def checkIndices(t: Array[Int]) = reverses.zip(t).exists(l => l._1 ^ l._2 == 1)

  def checkValues(t: Array[Int]) = checkIndices(t)

  override def toString = "\\/" + (scope, reverses).zipped.map((v, r) => (if (r) "-" else "") + v).mkString("(", ", ", ")")

  def revise(): List[Int] = {
    if (isTrue(watch1) || isTrue(watch2)) entail()
    else {
      if (isFalse(watch1)) {

        seekWatch(watch2) match {
          case Some(w) => {
            watch1 = w
            if (isTrue(w)) {
              entail()
              return Nil
            }
          }
          case None => {
            setTrue(watch2)
            entail()
            return watch2 :: Nil
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
            return watch1 :: Nil
          }
        }

      }
    }
    Nil
  }

  private def isTrue(position: Int) = {
    if (reverses(position)) domains(position).isFalse else domains(position).isTrue
  }

  private def isFalse(position: Int) =
    if (reverses(position)) domains(position).isTrue else domains(position).isFalse

  private def setTrue(position: Int) {
    if (canBeTrue(position)) {

      if (domains(position).isUnknown) {

        if (reverses(position)) {
          domains(position).setFalse()
        } else {
          domains(position).setTrue()
        }

      }
    } else {
      throw UNSATObject
    }

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
