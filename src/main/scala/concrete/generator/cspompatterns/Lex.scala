package concrete.generator.cspompatterns

import cspom.compiler.Delta
import cspom.CSPOM
import cspom.variable.CSPOMExpression
import cspom.variable.CSPOMSeq
import cspom.variable.BoolVariable
import cspom.compiler.ConstraintCompiler
import cspom.CSPOMConstraint
import scala.reflect.runtime.universe

object Lex extends ConstraintCompiler {

  type A = LexType

  sealed trait LexType
  object LexList extends LexType
  object LexMatrix extends LexType

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM): Option[A] = PartialFunction.condOpt(constraint.function) {
    case 'lex => LexList
    case 'lexmatrix => LexMatrix
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, data: A): Delta = {
    val mode = Symbol(constraint.getParam[String]("mode").get.toLowerCase)

    val Some((strict, arguments)) = constraint.getParam[String]("mode").map {
      case "LT" => (true, constraint.arguments)
      case "LE" => (false, constraint.arguments)
      case "GT" => (true, constraint.arguments.reverse)
      case "GE" => (false, constraint.arguments.reverse)
    }

    val constraints = data match {
      case LexList =>
        lexlist(constraint.arguments, strict)

      case LexMatrix =>

        val Seq(mat: CSPOMSeq[_]) = constraint.arguments
        val transposed = mat
          .map {
            case row: CSPOMSeq[_] => row.values
          }
          .transpose
          .map(CSPOMSeq(_: _*))

        lexlist(mat, strict) ++ lexlist(transposed, strict)

    }

    val reif = CSPOMConstraint(constraint.result)('and)(constraints.map(_.result): _*)
    replaceCtr(constraint, reif +: constraints, problem)
  }

  private def lexlist(list: Seq[CSPOMExpression[_]], strict: Boolean): Seq[CSPOMConstraint[_]] = {
    list.sliding(2)
      .map {
        case s =>
          CSPOMConstraint(new BoolVariable())('lexleq)(s: _*)
      }
      .toSeq
      .++ {
        if (strict) {
          list.sliding(2)
            .map {
              case s =>
                CSPOMConstraint(new BoolVariable())('nevec)(s: _*)
            }
            .toSeq
        } else {
          Seq()
        }
      }
  }

}