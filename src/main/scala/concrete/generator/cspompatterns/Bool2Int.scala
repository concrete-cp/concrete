package concrete.generator.cspompatterns

import cspom.CSPOMConstraint
import cspom.VariableNames
import cspom.compiler.VariableCompiler
import cspom.util.IntInterval._
import cspom.util.IntInterval
import cspom.util.IntervalsArithmetic._
import cspom.util.RangeSet
import cspom.variable.BoolVariable
import cspom.variable.CSPOMConstant
import cspom.variable.CSPOMExpression
import cspom.variable.IntVariable
import cspom.variable.IntExpression.implicits.arithmetics
import cspom.variable.IntExpression
import cspom.variable.IntExpression.implicits.ranges
import cspom.variable.SimpleExpression
import cspom.compiler.ConstraintCompilerNoData
import cspom.CSPOM
import cspom.util.Finite
import cspom.compiler.Delta
import cspom.variable.BoolExpression
import cspom.variable.CSPOMSeq

object Bool2IntDomains extends VariableCompiler('bool2int) {

  def compiler(c: CSPOMConstraint[_]) = c match {
    case CSPOMConstraint(CSPOMConstant(true), _, Seq(be: SimpleExpression[_], ie: SimpleExpression[_]), params) =>

      val b = BoolExpression.coerce(be)

      val iii = reduceDomain(IntExpression.coerce(ie), BoolExpression.span(b))

      val bb =
        if (iii.contains(0)) {
          assert(b.contains(false))
          if (iii.contains(1)) {
            assert(b.contains(true))
            b
          } else {
            CSPOMConstant(false)
          }
        } else if (iii.contains(1)) {
          assert(b.contains(true))
          CSPOMConstant(true)
        } else {
          throw new AssertionError()
        }

      Map(be -> bb, ie -> iii)

  }
}

object Bool2IntIsEq extends ConstraintCompilerNoData {
  def matchBool(c: CSPOMConstraint[_], p: CSPOM) =
    c.function == 'bool2int
  def compile(c: CSPOMConstraint[_], p: CSPOM) = {
    val Seq(b, i) = c.arguments.map(_.asInstanceOf[SimpleExpression[_]])
    logger.info(s"$b = $i")
    //(0).asInstanceOf[SimpleExpression[_]]
    // val i = c.arguments(1).asInstanceOf[SimpleExpression[_]]
    require(b.searchSpace == i.searchSpace &&
      (!b.contains(false) || i.contains(0) || i.contains(false)) && (
        !b.contains(true) || i.contains(1) || i.contains(true)))
    //println(s"removing $c")
    removeCtr(c, p) ++ (if (CSPOMSeq(c.arguments: _*).searchSpace > 1) replace(i, b, p) else Delta.empty)

  }
  def selfPropagation = false
}