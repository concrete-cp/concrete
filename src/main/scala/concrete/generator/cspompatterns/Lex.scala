package concrete.generator.cspompatterns

import cspom.compiler.{ConstraintCompiler, Delta, Functions}
import cspom.variable.{BoolVariable, CSPOMExpression, CSPOMSeq}
import cspom.{CSPOM, CSPOMConstraint}

object Lex extends ConstraintCompiler {

  type A = LexType

  def functions = Functions("lex", "lexmatrix")

  override def mtch(constraint: CSPOMConstraint[_], problem: CSPOM): Option[A] = Some {
    constraint.function match {
      case "lex" => LexList
      case "lexmatrix" => LexMatrix
    }
  }

  def compile(constraint: CSPOMConstraint[_], problem: CSPOM, data: A): Delta = {
    val Some((strict, arguments)) = constraint.getParam[String]("mode").map {
      case "LT" => (true, constraint.arguments)
      case "LE" => (false, constraint.arguments)
      case "GT" => (true, constraint.arguments.reverse)
      case "GE" => (false, constraint.arguments.reverse)
    }

    val constraints = data match {
      case LexList =>
        lexlist(arguments, strict)

      case LexMatrix =>

        val Seq(mat: CSPOMSeq[_]) = arguments
        val transposed = mat
          .map {
            case row: CSPOMSeq[_] => row.values
          }
          .transpose
          .map(CSPOMSeq(_: _*))

        lexlist(mat, strict) ++ lexlist(transposed, strict)

    }

    val reif = CSPOMConstraint(constraint.result)("and")(constraints.map(_.result): _*)
    ConstraintCompiler.replaceCtr(constraint, reif +: constraints, problem)
  }

  private def lexlist(list: Seq[CSPOMExpression[_]], strict: Boolean): Seq[CSPOMConstraint[_]] = {
    list.sliding(2)
      .map(s => CSPOMConstraint(new BoolVariable())("lexleq")(s: _*))
      .toSeq
      .++ {
        if (strict) {
          list.sliding(2)
            .map(
              s =>
                CSPOMConstraint(new BoolVariable())("nevec")(s: _*)
            )
            .toSeq
        } else {
          Seq()
        }
      }
  }

  sealed trait LexType

  object LexList extends LexType

  object LexMatrix extends LexType

}