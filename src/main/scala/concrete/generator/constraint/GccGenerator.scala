package concrete.generator.constraint;

import concrete.Problem
import concrete.Variable
import concrete.constraint.semantic.Bounds
import concrete.constraint.semantic.Gcc
import cspom.CSPOMConstraint

final class GccGenerator(problem: Problem) extends AbstractGenerator(problem) {
  override def gen(constraint: CSPOMConstraint) = {
    val scope = constraint.arguments map cspom2concreteVar

    if (scope.exists(_.dom.undefined)) {
      false
    } else {
      val bounds = constraint.params.get("gcc") match {
        case Some(p: Seq[(Int, Int, Int)]) => p.map(i => Bounds(i._1, i._2, i._3)).toArray
        case Some(p: Array[(Int, Int, Int)]) => p.map(i => Bounds(i._1, i._2, i._3))
        case p: Any => AbstractGenerator.fail(
          s"GCC constraints requires to be parameterized with a sequence of triples, found $p")
      }

      addConstraint(new Gcc(scope.toArray, bounds));
      true;
    }

  }
}
