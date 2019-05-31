package concrete.generator.cspompatterns

import concrete.constraint.linear.SumMode
import concrete.generator.SumGenerator
import cspom.compiler.{CompiledFunctions, ConstraintCompiler, Delta, Functions}
import cspom.variable.CSPOMExpression
import cspom.{CSPOM, CSPOMConstraint}

/**
  * Find and merge auxiliary variables similary defined by other constraints
  */
object MergeSameSum extends ConstraintCompiler {

  type A = Seq[(CSPOMExpression[_], CSPOMConstraint[_], CSPOMExpression[_])]

  def functions: CompiledFunctions = Functions('sum)

  override def mtch(c1: CSPOMConstraint[_], problem: CSPOM): Option[A] = {
    if (c1.result.isTrue) {
      val (args1, factors1, const1, mode1) = SumGenerator.readCSPOM(c1)
      if (mode1 == SumMode.EQ) {
        val candidates = for {
          arg <- args1
          c2 <- problem.deepConstraints(arg)
          if (c2 ne c1) && c2.result.isTrue && c2.function == 'sum
        } yield c2

        val s = for {
          c2 <- candidates.distinct
          (args2, factors2, const2, mode2) = SumGenerator.readCSPOM(c2)
          if mode2 == SumMode.EQ && const2 == const1
          //_ = println(s"${args1.map(System.identityHashCode)} ${args2.map(System.identityHashCode)}, ${oneDiff(args1 zip factors1, args2 zip factors2)}")
          ((same1, _), (same2, _)) <- oneDiff(args1 zip factors1, args2 zip factors2)
        } yield (same1, c2, same2)

        if (s.nonEmpty) {
          Some(s)
        } else {
          None
        }
      } else {
        None
      }
    } else {
      None
    }

  }

  // Iff the two sequences contain the same elements but one, return that element
  private def oneDiff[B <: AnyRef](a: Seq[B], b: Seq[B]): Option[(B, B)] = {

    if (a.size != b.size) {
      None
    } else {
      val as = a.toSet -- b
      val bs = b.toSet -- a

      if (as.size == 1 && bs.size == 1) {
        Some((as.head, bs.head))
      } else {
        None
      }

    }


  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, same: A): Delta = {
    same.foldLeft(Delta()) { case (delta, (a1, c2, a2)) =>
      val eqC = CSPOMConstraint('eq)(a1, a2)
      delta ++ ConstraintCompiler.replaceCtr(c2, eqC, problem)
    }

  }

  def selfPropagation = false

}
