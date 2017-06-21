package concrete.generator.cspompatterns

import concrete.SumBuilder
import cspom.compiler.{ConstraintCompilerNoData, Delta}
import cspom.variable.{CSPOMConstant, CSPOMExpression, CSPOMSeq, IntExpression}
import cspom.{CSPOM, CSPOMConstraint}

/**
  * @author vion
  */
object GCC extends ConstraintCompilerNoData {

  def matchBool(c: CSPOMConstraint[_], p: CSPOM) = (c.function == 'gccExact || c.function == 'gccMinMax) && c.result == CSPOMConstant(true)

  def compile(c: CSPOMConstraint[_], p: CSPOM): Delta = {

    c.function match {
      case 'gccExact =>
        val CSPOMConstraint(_, _, Seq(vars: CSPOMSeq[_], CSPOMConstant(closed: Boolean), values: CSPOMSeq[_], IntExpression.simpleSeq(occ)), params) = c

        val constraints = for ((v, o) <- (values, occ).zipped.toSeq) yield {
          CSPOMConstraint(o)('occurrence)(v, vars)
        }

        val closedCons: Seq[CSPOMConstraint[_]] = if (closed) {
          val in = closedConstraints(vars, values)
          val implied = occ.map(SumBuilder(_)).reduce(_ + _) === vars.length
          implied +: in
        } else {
          val implied = occ.map(SumBuilder(_)).reduce(_ + _) <= vars.length
          Seq(implied)
        }

        replaceCtr(c, constraints ++ closedCons, p)

      case 'gccMinMax =>
        val CSPOMConstraint(_, _, Seq(vars: CSPOMSeq[_], CSPOMConstant(closed: Boolean), values: CSPOMSeq[_], IntExpression.simpleSeq(occMin), IntExpression.simpleSeq(occMax)), params) = c

        val constraints =
          for {(v, min, max) <- (values, occMin, occMax).zipped.toSeq
               c <- Seq(
                 CSPOMConstraint('atLeast)(min, v, vars),
                 CSPOMConstraint('atMost)(max, v, vars))
          } yield c

        val closedCons: Seq[CSPOMConstraint[_]] = if (closed) {
          val in = closedConstraints(vars, values)
          val implied = Seq(
            occMin.map(SumBuilder(_)).reduce(_ + _) <= vars.length,
            occMax.map(SumBuilder(_)).reduce(_ + _) >= vars.length)

          implied ++: in
        } else {
          Seq()
        }

        replaceCtr(c, constraints ++ closedCons, p)
    }

  }

  private def closedConstraints(vars: Seq[CSPOMExpression[_]], values: CSPOMSeq[_]) = {
    for (v <- vars) yield {
      CSPOMConstraint('in)(v, values)
    }
  }
}