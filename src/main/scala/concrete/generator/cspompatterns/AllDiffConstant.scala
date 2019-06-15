package concrete.generator.cspompatterns

import concrete.CSPOMDriver
import cspom.compiler.ConstraintCompiler.{addCtr, applyDomain, removeCtr, replace}
import cspom.compiler.{ConstraintCompiler, Delta, Functions}
import cspom.util.{Finite, Infinitable, IntInterval, RangeSet}
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.{CSPOMConstant, IntExpression, SimpleExpression}
import cspom.{CSPOM, CSPOMConstraint}

import scala.collection.mutable

/**
  * Removes constants from alldifferent constraints
  */
object AllDiffConstant extends ConstraintCompiler {
  type A = (Seq[Int], Seq[SimpleExpression[Int]])

  def functions = Functions("alldifferent")

  override def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[A] = {
    c match {
      case CSPOMConstraint(CSPOMConstant(true), _, arguments: Seq[SimpleExpression[Int]] @unchecked, _) =>

        val (constants, variables) = pickAndRemoveConstants(arguments, ranges)
        if (constants.nonEmpty) {
          Some((constants, variables))
        } else {
          None
        }

      case _ => None
    }
  }

  def pickAndRemoveConstants(args: Seq[SimpleExpression[Int]], dom: SimpleExpression[Int] => RangeSet[Infinitable]): (Seq[Int], Seq[SimpleExpression[Int]]) = {
    val (constants, variables) = args.partition { v =>
      dom(v).span.itvSize == Finite(1)
    }

    (constants.map(dom(_).lowerBound).map {
      case Finite(lb) => lb
      case e => throw new IllegalArgumentException(s"$e not supported")
    }, variables)
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, constants: A): Delta = {
    val except = constraint.getSeqParam[Int]("except").toSet

    val map = mutable.Map[SimpleExpression[Int], RangeSet[Infinitable]]() ++ constraint.arguments.map {
      case v: SimpleExpression[Int] @unchecked => v -> ranges(v)
      case e => throw new IllegalArgumentException(s"$e not supported")
    }

    def filter(constants: Seq[Int], variables: Seq[SimpleExpression[Int]]): Seq[SimpleExpression[Int]] = {
      if (constants.isEmpty) {
        variables
      } else {
        val toRemove = RangeSet(constants.filterNot(except).map(IntInterval.singleton))
        for (v <- variables) {
          val actualDomain = map(v)
          map(v) = actualDomain -- toRemove
        }
        (filter _).tupled(pickAndRemoveConstants(variables, map))
      }
    }

    val (constant, args) = constants

    val remaining = filter(constant, args)

    var delta = removeCtr(constraint, problem)

    if (remaining.lengthCompare(1) > 0) {
      delta ++= addCtr(CSPOMDriver.allDifferent(remaining: _*) withParams constraint.params, problem)
    }

    for ((v, f) <- map if IntExpression.implicits.ranges(v) != f) {
      delta ++= replace(v, applyDomain(v, f), problem)
    }

    delta
  }
}
