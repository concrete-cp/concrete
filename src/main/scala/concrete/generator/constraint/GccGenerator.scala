package concrete.generator.constraint;

import concrete.Problem
import concrete.Variable
import concrete.constraint.semantic.Bounds
import concrete.constraint.semantic.Gcc
import cspom.CSPOMConstraint
import Generator._

final object GccGenerator extends Generator {
  override def gen(constraint: CSPOMConstraint[Boolean])(implicit variables: VarMap) = {
    val scope = constraint.arguments map cspom2concreteVar

    val bounds = constraint.params.get("gcc") match {
      case Some(p: Seq[(Int, Int, Int)]) => p.map(i => Bounds(i._1, i._2, i._3)).toArray
      case Some(p: Array[(Int, Int, Int)]) => p.map(i => Bounds(i._1, i._2, i._3))
      case p: Any => Generator.fail(
        s"GCC constraints requires to be parameterized with a sequence of triples, found $p")
    }

    Seq(new Gcc(scope.toArray, bounds))

  }
}
