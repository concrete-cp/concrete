package concrete
package generator

import concrete.constraint.Constraint
import concrete.constraint.semantic.{AllDifferent2C, AllDifferentBC, Neq}
import concrete.generator.Generator.cspom2concreteVar
import cspom.CSPOMConstraint

final class AllDifferentGenerator(val pg: ProblemGenerator) extends Generator {

  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap): Seq[Constraint] = {
    constraint.getParam[Seq[Int]]("except") match {
      case Some(except) => Seq(new AllDifferent2C(constraint.arguments.map(cspom2concreteVar(_)).toArray, except.toSet))
      case None =>
        constraint.arguments.map(cspom2concreteVar(_)) match {
          case Seq() | Seq(_) => Seq()
          case Seq(v0, v1) => Seq(new Neq(v0, v1))
          case v: Seq[Variable] => {
            val tc =
              if (pg.pm.contains("alldifferent.useclique")) {
                for (Seq(x, y) <- v.combinations(2)) yield new Neq(x, y)
              } else {
                Seq(new AllDifferent2C(v.toArray))
              }

            new AllDifferentBC(v: _*) +: tc.toSeq
          }
        }
    }
  }

}
