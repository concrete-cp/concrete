package concrete.generator.cspompatterns

import concrete.generator.cspompatterns.Polarity.{Negative, Positive}
import cspom.CSPOM.seq2CSPOMSeq
import cspom.CSPOMConstraint
import cspom.compiler.ACCSE
import cspom.variable.{CSPOMVariable, SimpleExpression}

// case class Clause(pos: Set[CSPOMExpression[Any]], neg: Set[CSPOMExpression[Any]])

sealed trait Polarity
object Polarity {

  case object Positive extends Polarity

  case object Negative extends Polarity

}

object ACCSE_Clause extends ACCSE[Polarity] {

  def functions: Seq[Symbol] = Seq('clause)


  override def canonize(args: List[Arg]): List[Arg] = args

  override def define(subexp: List[Arg], aux: CSPOMVariable[_]): (Arg, CSPOMConstraint[_]) = {
    val (negative, positive) = subexp.partition(_._2 == Negative)
    val constraint = CSPOMConstraint(aux)('clause)(seq2CSPOMSeq(positive.map(_._1)), seq2CSPOMSeq(negative.map(_._1)))
    val arg = (aux, Positive)
    (arg, constraint)
  }

  override def replace(subexp: List[Arg], arg: Arg, constraint: Args): Option[Arg] = {
    if (subexp.forall { case (expr, polarity) => constraint(expr) == polarity }) {
      constraint --= subexp.map(_._1)
      constraint += arg
      Some(arg)
    } else {
      None
    }
  }

  override def intersect(se1: List[Arg], se2: List[Arg], including: List[Arg]): List[Arg] = {
    val se1Map = se1.toSet
    assert(including.forall(se1Map.contains))
    assert {
      val se2Map = se2.toSet
      including.forall(se2Map.contains)
    }

    val additional1 = se1Map -- including
    val additional2 = se2.filter(additional1)
    including ++ additional2
  }

  override def constraintToArgs(c: CSPOMConstraint[_]): IndexedSeq[Arg] = {
    val Seq(SimpleExpression.simpleSeq(positive), SimpleExpression.simpleSeq(negative)) = c.arguments
    positive.map((_, Positive)) ++ negative.map((_, Negative))
  }

  override def argsToConstraint(original: CSPOMConstraint[_], args: Args): CSPOMConstraint[_] = {
    val (negative, positive) = args.partition(_._2 == Negative)
    CSPOMConstraint(original.result)('clause)(seq2CSPOMSeq(positive.keys), seq2CSPOMSeq(negative.keys))
  }
}