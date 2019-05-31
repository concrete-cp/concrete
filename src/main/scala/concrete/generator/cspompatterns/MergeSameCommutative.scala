package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, Delta, Functions}

/**
 * Find and merge auxiliary variables similary defined by other constraints
 */
object MergeSameCommutative extends ConstraintCompiler {

  def functions = Functions('eq, 'ne, 'mul, 'nevec)

  type A = Seq[CSPOMConstraint[_]]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM): Option[A] = {
    val s = for {
      arg <- c.arguments
      argCons <- problem.constraints(arg)
      if (argCons ne c) && c.result != argCons.result &&
        c.function == argCons.function &&
        isSame(c.arguments, argCons.arguments) &&
        c.params == argCons.params
    } yield argCons

    if (s.nonEmpty) {
      Some(s)
    } else {
      None
    }
  }

  private def isSame[B <: AnyRef](a: Seq[B], b: Seq[B]): Boolean = {
    a.toSet == b.toSet
//    val ai = a.iterator
//    val bi = b.iterator
//
//    while (ai.hasNext && bi.hasNext) {
//      if (ai.next() ne bi.next()) return false
//    }
//
//    ai.isEmpty && bi.isEmpty

  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, same: A): Delta = {

    val eqC = CSPOMConstraint('eq)(c.result +: same.map(_.result): _*)
    ConstraintCompiler.replaceCtr(same.distinct, eqC, problem)

  }

  def selfPropagation = false

}
