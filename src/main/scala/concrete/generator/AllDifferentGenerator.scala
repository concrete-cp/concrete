package concrete
package generator

import concrete.constraint.Constraint
import concrete.constraint.semantic.{AllDifferent2C, AllDifferentAC, AllDifferentBC, Neq}
import concrete.generator.Generator.cspom2concreteVar
import cspom.CSPOMConstraint

final class AllDifferentGenerator(val pg: ProblemGenerator) extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap): Seq[Constraint] = {
    val except = constraint.getSeqParam[Int]("except")

    val vars = constraint.arguments.map(cspom2concreteVar(_))

    if (except.nonEmpty) {
      Seq(new AllDifferent2C(vars.toArray, except.toSet))
    } else {
      vars match {
        case Seq() | Seq(_) => Seq()
        case Seq(v0, v1) => Seq(new Neq(v0, v1))
        case v: Seq[Variable] =>
          val tc =
            if (pg.pm.contains("alldifferent.useclique")) {
              for (Seq(x, y) <- v.combinations(2)) yield new Neq(x, y)
            } else {
              Seq(new AllDifferent2C(v.toArray))
            }

          val allValues = v.flatMap(_.initDomain).distinct

          val hc = if (allValues.size <= 1.5 * v.size) {
            new AllDifferentAC(v.toArray)
          } else {
            new AllDifferentBC(v.toArray)
          }

          hc +: tc.toSeq

      }

    }

  }
}
