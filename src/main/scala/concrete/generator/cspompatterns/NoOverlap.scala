package concrete.generator.cspompatterns

import cspom.CSPOM
import cspom.CSPOMConstraint
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta}
import cspom.variable.CSPOMConstant
import cspom.variable.SimpleExpression
import cspom.variable.BoolVariable
import concrete.CSPOMDriver
import CSPOM._
import ConstraintCompiler._

/**
  * @author vion
  */
object NoOverlap extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = c.function == 'noOverlap

  def compile(c: CSPOMConstraint[_], p: CSPOM): Delta = {
    val CSPOMConstraint(CSPOMConstant(true), _, Seq(SimpleExpression.simpleSeq(origins), SimpleExpression.simpleSeq(lengths)), _) = c

    // val zeroIgnored = c.getParam[Boolean]("zeroIgnored").get
    val cumulative = false
    if (cumulative) {
      val constraint = CSPOMConstraint('cumulative)(origins, lengths, seq2CSPOMSeq(Seq.fill(origins.length)(CSPOMConstant(1))), 1)
      replaceCtr(c, constraint, p)
    } else {
      val constraints = for (i <- origins.indices; j <- origins.indices if i < j) yield {
        //if (!zeroIgnored || (lengths(i) != 0 && lengths(j) != 0)) {
        val choice1 = new BoolVariable()
        val choice2 = new BoolVariable()
        Seq(
          CSPOMDriver.clause(choice1, choice2)(),
          CSPOMConstraint(choice1)('sum)(Seq(1, -1, 1), Seq(origins(i), origins(j), lengths(i)), 0) withParam "mode" -> "le",
          CSPOMConstraint(choice2)('sum)(Seq(-1, 1, 1), Seq(origins(i), origins(j), lengths(j)), 0) withParam "mode" -> "le")
        //}
      }

      replaceCtr(c, constraints.flatten, p)
    }
  }
}