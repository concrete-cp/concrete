package cspfj.constraint.semantic;

import java.util.Arrays
import cspfj.constraint.AbstractConstraint
import cspfj.filter.RevisionHandler
import cspfj.problem.BooleanDomain
import cspfj.problem.Variable;
import cspfj.constraint.SimpleRemovals

final class Disjunction(scope: Array[Variable],
  val reverses: IndexedSeq[Boolean]) extends AbstractConstraint(null, scope)
  with SimpleRemovals {

  require(scope forall (v => v.dom.isInstanceOf[BooleanDomain] && v.dom.size == 2),
    "Only non-constant boolean domains are allowed")
  require(reverses != null)
  require(reverses.size == scope.size, "reverses must cover all variables")

  val domains = scope map (_.dom.asInstanceOf[BooleanDomain])

  var watch1 = seekWatch(-1);
  require(watch1 >= 0, "Unexpected inconsistency")
  var watch2 = seekWatch(watch1);
  if (watch2 < 0) {
    setTrue(watch1)
    watch2 = watch1
  }

  val getEvaluation = math.log(arity) / math.log(2)

  def this(scope: Variable*) = this(scope.toArray, new Array[Boolean](scope.size))

  def check = reverses.zip(tuple).exists(l => l._1 ^ l._2 == 1)

  override def toString = "\\/" + scope.mkString("(", ", ", ")")

  override def revise(revisator: RevisionHandler, reviseCount: Int): Boolean = {
    if (isFalse(watch1)) {
      val newWatch = seekWatch(watch2);
      if (newWatch < 0) {
        if (!canBeTrue(watch2)) {
          return false;
        }
        if (setTrue(watch2)) {
          revisator.revised(this, scope(watch2));
          return true;
        }
      } else {
        watch1 = newWatch;
      }
    }
    if (isFalse(watch2)) {
      val newWatch = seekWatch(watch1);
      if (newWatch >= 0) {
        watch2 = newWatch;
      } else if (setTrue(watch1)) {
        revisator.revised(this, scope(watch1));
      }
    }
    return true;
  }

  private def isFalse(position: Int) =
    if (reverses(position)) domains(position).isTrue else domains(position).isFalse

  private def setTrue(position: Int) = {
    val dom = domains(position)
    if (dom.isUnknown) {
      if (reverses(position)) {
        dom.setFalse()
      } else {
        dom.setTrue()
      }
      true
    } else false

  }

  private def canBeTrue(position: Int) = domains(position).canBe(!reverses(position));

  private def seekWatch(excluding: Int) =
    (0 until arity).find(i => i != excluding && canBeTrue(i)) match {
      case Some(i) => i
      case None => -1
    }

}
