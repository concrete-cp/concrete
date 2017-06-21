package concrete.generator.cspompatterns

import concrete.constraint.linear.SumMode
import concrete.generator.SumGenerator
import cspom.CSPOM.{constant, constantSeq, seq2CSPOMSeq}
import cspom.compiler.ConstraintCompiler
import cspom.variable.{BoolExpression, CSPOMExpression}
import cspom.{CSPOM, CSPOMConstraint}

/**
  * Merge duplicates in linear constraints (x + x = 0 -> 2.x = 0). Also remove variables with factor = 0.
  */
object SumDuplicates extends ConstraintCompiler {

  type A = (CSPOMExpression[_], collection.Map[CSPOMExpression[Any], Int], Int)

  override def mtch(c: CSPOMConstraint[_], p: CSPOM) = {
    if (c.function == 'sum) {
      val (vars, coefs, const, mode) = SumGenerator.readCSPOM(c)

      var duplicates = false
      val factors = collection.mutable.Map[CSPOMExpression[_], Int]()
      for ((v, c) <- (vars, coefs).zipped) {
        factors.get(v) match {
          case Some(i) =>
            duplicates = true
            factors(v) = i + c
          case None =>
            factors(v) = c
        }
      }

      if (duplicates || factors.values.exists(_ == 0)) {
        Some((c.result, factors, const))
      } else {
        None
      }

    } else {
      None
    }
  }


  def compile(constraint: CSPOMConstraint[_], p: CSPOM, data: A) = {
    val (r, args, const) = data
    val mode = constraint.getParam[String]("mode").flatMap(SumMode.withName).get
    val (variables, factors) = args.filter(_._2 != 0).unzip

    if (factors.isEmpty) {
      val truth = SumConstants.checkConstant(const, mode)
      logger.info(s"Linear constraint with no variables: $constraint, entailed to $truth")

      val nr = reduceDomain(BoolExpression.coerce(constraint.result), truth)
      removeCtr(constraint, p) ++ replace(constraint.result, nr, p)

    } else {
      val newConstraint =
        CSPOMConstraint(r)('sum)(factors.toSeq, variables.toSeq, const) withParams constraint.params

      //println(s"replacing $constraint with $newConstraint")
      replaceCtr(constraint, newConstraint, p)

    }

  }

  def selfPropagation = false

}