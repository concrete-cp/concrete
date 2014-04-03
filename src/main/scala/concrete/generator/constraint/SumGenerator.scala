package concrete.generator.constraint;

import scala.collection.mutable.HashMap
import concrete.Problem
import concrete.Variable
import concrete.constraint.extension.MDD
import concrete.constraint.extension.MDD0
import concrete.constraint.extension.MDDLeaf
import concrete.constraint.semantic.AllDifferent2C
import concrete.constraint.semantic.AllDifferentBC
import concrete.constraint.semantic.Sum
import cspom.CSPOMConstraint
import concrete.constraint.extension.MDDn
import concrete.constraint.extension.ReduceableExt
import scala.annotation.tailrec
import concrete.generator.FailedGenerationException
import Generator._
import concrete.constraint.semantic.FilterSum
import cspom.variable.CSPOMSeq
import concrete.constraint.semantic.FilterSum
import concrete.constraint.semantic.FilterSum
import concrete.util.Interval
import cspom.variable.CSPOMConstant

final object SumGenerator extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val Seq(CSPOMSeq(vars, _, _), CSPOMConstant(const: Int)) = constraint.arguments //map cspom2concreteVar

    val params = constraint.params.get("coefficients") match {
      case Some(p: Seq[_]) => p.asInstanceOf[Seq[Int]]
      case None => Seq.fill(vars.length)(1)
      case _ => throw new IllegalArgumentException("Parameters for zero sum must be a sequence of integer values")
    }

    val (solverVariables, varParams) = (vars map cspom2concrete).zip(params).collect {
      case (Var(v), p) => (v, p)
    } unzip

    val constant = const - (vars map cspom2concrete).zip(params).collect {
      case (Const(c), p) => c * p
    }.sum

    val mode = constraint.params.get("mode").collect { case m: String => FilterSum.withName(m) }.get

    undefinedVar(solverVariables: _*) match {
      case Seq() => go(constant, varParams, solverVariables, mode)
      case Seq(uv) if mode == FilterSum.SumEQ =>
        val min = (solverVariables zip varParams).map { case (v, p) => if (v eq uv) 0 else v.dom.firstValue * p }.sum
        val max = (solverVariables zip varParams).map { case (v, p) => if (v eq uv) 0 else v.dom.lastValue * p }.sum
        val factor = varParams(solverVariables.indexOf(uv))
        Generator.restrictDomain(uv, ((Interval(min, max) - constant) / -factor).allValues.iterator)
        go(constant, varParams, solverVariables, mode)
      case _ => None
    }

  }

  def go(c: Int, params: Seq[Int], solverVariables: Seq[Variable], mode: FilterSum.Value) =
    Some(Seq(new Sum(c, params.toArray, solverVariables.toArray, mode)))

}
