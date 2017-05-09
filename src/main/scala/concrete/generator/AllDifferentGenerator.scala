package concrete
package generator

import Generator.cspom2concreteVar
import concrete.constraint.Constraint
import concrete.constraint.semantic.AllDifferentBC
import concrete.constraint.semantic.Neq
import cspom.CSPOMConstraint
import concrete.constraint.semantic.AllDifferent2C

final class AllDifferentGenerator(val pg: ProblemGenerator) extends Generator {
  
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap): Seq[Constraint] = {
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
