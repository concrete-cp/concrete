package concrete.generator.cspompatterns

import cspom.CSPOM._
import cspom.CSPOMConstraint
import cspom.compiler.ACCSE
import cspom.variable._

sealed trait MinMaxType {
  def function: String
}

case object MinType extends MinMaxType {
  def function = "min"
}

case object MaxType extends MinMaxType {
  def function = "max"
}

object ACCSE_MinMax extends ACCSE[MinMaxType] {

  def functions: Seq[String] = Seq("min", "max")

  override def define(subexp: List[Arg], aux: CSPOMVariable[_]): (Arg, CSPOMConstraint[_]) = {
    val typ = subexp.head._2
    assert(subexp.forall(_._2 == typ))
    val constraint = CSPOMConstraint(aux)(typ.function)(subexp.map(_._1): _*)
    val arg = (aux, typ)
    (arg, constraint)
  }

  override def replace(subexp: List[Arg], arg: Arg, constraint: Args): Option[Arg] = {
    assert(subexp.forall(_._2 == arg._2))
    assert(constraint.values.forall(_ == arg._2))

    if (subexp.forall { case (expr, typ) => constraint(expr).contains(typ) }) {
      constraint --= subexp.map(_._1)
      constraint += arg
      Some(arg)
    } else {
      None
    }
  }

  override def constraintToArgs(c: CSPOMConstraint[_]): IndexedSeq[Arg] = {
    c.function match {
      case "min" => c.arguments.map(v => (v, MinType)).toIndexedSeq
      case "max" => c.arguments.map(v => (v, MaxType)).toIndexedSeq
    }
  }

  override def argsToConstraint(original: CSPOMConstraint[_], args: ACCSE_MinMax.Args): CSPOMConstraint[_] = {
    val typ = args.head._2
    require(args.forall(_._2 == typ))
    CSPOMConstraint(original.result)(typ.function)(args.keys: _*)
  }

  override def canonize(args: List[Arg]): List[Arg] = args

  override def intersect(se1: List[Arg], se2: List[Arg], including: List[Arg]): List[Arg] = {
    val typ = se1.head._2
    assert(se1.forall(_._2 == typ))
    assert(se2.forall(_._2 == typ))

    val se1Set = se1.toSet
    assert(including.forall(se1.contains))
    assert {
      val se2Set = se2.toSet
      including.forall(se2Set.contains)
    }

    val additional1 = se1Set -- including
    val additional2 = se2.filter(additional1)
    including ++ additional2
  }
}