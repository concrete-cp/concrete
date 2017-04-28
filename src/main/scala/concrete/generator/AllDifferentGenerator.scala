package concrete.generator;

import Generator.cspom2concreteVar
import concrete.ParameterManager
import concrete.constraint.semantic.AllDifferentBC
import concrete.constraint.semantic.Neq
import cspom.CSPOMConstraint
import concrete.constraint.semantic.AllDifferent2C
import concrete.Variable
import concrete.constraint.Constraint

final class AllDifferentGenerator(val pg: ProblemGenerator) extends Generator {
  
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap) = {
    constraint.arguments.map(cspom2concreteVar) match {
      case Seq() | Seq(_) => Seq()
      case Seq(v0, v1) => Seq(new Neq(v0, v1))
      case v => {
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
