package concrete.generator.cspompatterns

import concrete.constraint.linear._
import concrete.generator.SumGenerator
import cspom.CSPOM._
import cspom.compiler.ConstraintCompiler._
import cspom.compiler.{ConstraintCompiler, ConstraintCompilerNoData, Delta}
import cspom.variable.{BoolExpression, CSPOMConstant, IntExpression}
import cspom.{CSPOM, CSPOMConstraint}

/**
  * Remove constants from linear constraints
  */
object SumGreaterToLesser extends ConstraintCompiler {
  type A = Seq[Int]

  override def mtch(c: CSPOMConstraint[_], p: CSPOM): Option[Seq[Int]] = {
    if (c.function == 'sum && c.getParam[Any]("mode").exists(mode => mode == "ge" || mode == "gt")) {
      IntExpression.constSeq.unapply(c.arguments(0))
    } else {
      None
    }
  }

  def compile(c: CSPOMConstraint[_], p: CSPOM, constants: A): Delta = {
    val CSPOMConstant(k: Int) = c.arguments(2)

    val newMode = c.getParam[String]("mode")
      .map {
        case "ge" => "le"
        case "gt" => "lt"
      }
      .get

    val newConstraint = CSPOMConstraint(c.result)('sum)(constants.map(-_), c.arguments(1), -k) withParams (c.params + ("mode" -> newMode))

    replaceCtr(c, newConstraint, p)


  }

}