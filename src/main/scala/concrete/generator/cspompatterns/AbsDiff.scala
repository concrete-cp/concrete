package concrete.generator.cspompatterns
import cspom.CSPOM
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import cspom.variable.CSPOMVariable
import cspom.compiler.Delta
import cspom.variable.CSPOMExpression
import cspom.compiler.ConstraintCompilerNoData
import concrete.generator.constraint.Generator
import cspom.variable.SimpleExpression
import concrete.util.Interval
import cspom.variable.IntVariable

/**
 * If constraint is the sub() constraint, converts a=sub(y,z), x=abs(a) to
 * x=absdiff(y,z). No other constraint may imply the auxiliary constraint a.
 */
object AbsDiff extends ConstraintCompiler {
  type A = Set[CSPOMConstraint[Int]]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM) = c match {
    case CSPOMConstraint(result: CSPOMVariable[Int], 'sub, args, _) if result.hasParam("var_is_introduced") =>
      val process: Set[CSPOMConstraint[Int]] = problem.constraints(result).collect {
        case c @ CSPOMConstraint(_, 'abs, Seq(result), _) => c.asInstanceOf[CSPOMConstraint[Int]]
      }
      if (process.isEmpty) {
        None
      } else {
        Some(process)
      }
    case _ => None
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    data.foldLeft(Delta()) { (acc, fc) =>
      val nc = CSPOMConstraint(fc.result, 'absdiff, c.arguments)
      acc ++ replaceCtr(Seq(c, fc), nc, problem)
    }

  }

  def selfPropagation = false
}

object AbsDiffDomains extends ConstraintCompiler {
  type A = SimpleExpression[Int]

  override def mtch(c: CSPOMConstraint[_], problem: CSPOM) = {
    if (c.function == 'absdiff) {
      val undefined = (c.result +: c.arguments).filterNot(_.fullyDefined).map(_.asInstanceOf[SimpleExpression[Int]])
      if (undefined.size == 1) {
        Some(undefined.head)
      } else {
        None
      }
    } else {
      None
    }
  }

  private def toInterval(s: SimpleExpression[Int]) = {
    Interval(s.domain.head, s.domain.last)
  }

  private def union(i0: Interval, i1: Interval) = Interval(math.min(i0.lb, i1.lb), math.max(i0.ub, i1.ub))

  private def toIntVariable(i: Interval) = {
    IntVariable(i.lb to i.ub)
  }

  def compile(c: CSPOMConstraint[_], problem: CSPOM, data: A) = {
    val Seq(a0: SimpleExpression[Int], a1: SimpleExpression[Int]) = c.arguments
    val result = c.result.asInstanceOf[SimpleExpression[Int]]

    data match {
      case `result` =>
        val i0 = toInterval(a0)
        val i1 = toInterval(a1)
        val r = (i0 - i1).abs
        val s = collection.mutable.SortedSet[Int]()
        for (i <- a0.domain; j <- a1.domain) {
          s += math.abs(i - j)
        }
        replace(Seq(result), IntVariable(s.toSeq), problem)

      case `a0` =>
        val r = toInterval(result)
        val i1 = toInterval(a1)
        val i0 = union(i1 + r, i1 - r)
        replace(Seq(a0), toIntVariable(i0), problem)

      case `a1` =>
        val r = toInterval(result)
        val i0 = toInterval(a0)
        val i1 = union(i0 + r, i0 - r)
        replace(Seq(a1), toIntVariable(i1), problem)

      case _ => throw new IllegalStateException
    }
  }

  def selfPropagation = true

}