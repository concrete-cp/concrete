package concrete.generator.cspompatterns

import concrete.{SumBuilder, util}
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta, Functions}
import cspom.variable._
import cspom.{CSPOM, CSPOMConstraint}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by vion on 02/06/17.
  */
object NonLinearSum extends ConstraintCompilerNoData {

  def functions = Functions('sum)

  def matchBool(c: CSPOMConstraint[_], p: CSPOM): Boolean = {
    val SimpleExpression.simpleSeq(coefs) = c.arguments(0)
    coefs.exists(_.searchSpace > 1)
  }

  def compile(constraint: CSPOMConstraint[_], p: CSPOM): Delta = {
    //implicit def prob = p

    val (vars, varCoefs, constant, mode) = readCSPOM(constraint)

    val muls = new ArrayBuffer[CSPOMConstraint[_]]()

    val (neg, k, revMode) = mode match {
      case "ge" => (-1, -constant, "le")
      case "gt" => (-1, -constant, "lt")
      case e => (1, constant, e)
    }

    val arguments = (varCoefs, vars).zipped.map {
      case (c: CSPOMVariable[_], v) =>
        val r = IntVariable.free()
        val constraint = CSPOMConstraint(r)('mul)(c, v)
        muls += constraint
        SumBuilder(neg, r)
      case (CSPOMConstant(c), v) =>
        SumBuilder(c * neg, v)
    }


    val linear = arguments.reduce(_ + _) === k withParam "mode" -> revMode
    ConstraintCompiler.replaceCtr(constraint, linear +: muls, p)
  }

  private def readCSPOM(constraint: CSPOMConstraint[_]) = {
    require(constraint.arguments.size == 3)
    val IntExpression.simpleSeq(coefs) = constraint.arguments(0)
    val IntExpression.simpleSeq(vars) = constraint.arguments(1)
    val CSPOMConstant(c) = constraint.arguments(2) //map cspom2concreteVar

    // For bool2int optimization
    val constant = util.Math.any2Int(c)

    val mode = constraint.getParam[String]("mode")
      .getOrElse(throw new IllegalArgumentException("Constraint " + constraint + " has no valid mode"))

    (vars, coefs, constant, mode)

  }

}