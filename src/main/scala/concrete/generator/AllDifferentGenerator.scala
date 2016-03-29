package concrete.generator;

import Generator.cspom2concreteVar
import concrete.ParameterManager
import concrete.constraint.semantic.AllDifferentBC
import concrete.constraint.semantic.Neq
import cspom.CSPOMConstraint
import concrete.constraint.semantic.AllDifferent2C

final class AllDifferentGenerator(pm: ParameterManager) extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit varMap: VarMap) = {
    val vars = constraint.arguments.map(cspom2concreteVar)

    vars match {
      case Seq() | Seq(_) => Seq()
      case Seq(v0, v1)    => Seq(new Neq(v0, v1))
      case v => {
        val tc =
          if (pm.contains("alldifferent.useclique")) {
            for (Seq(x, y) <- v.combinations(2)) yield new Neq(x, y)
          } else {
            Seq(new AllDifferent2C(v: _*))
          } 

        new AllDifferentBC(v: _*) +: tc.toSeq
      }
    }

  }

}
