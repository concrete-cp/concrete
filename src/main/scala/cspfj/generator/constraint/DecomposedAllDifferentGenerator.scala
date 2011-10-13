package cspfj.generator.constraint;

import cspfj.constraint.semantic.{ SumLeq, ReifiedConstraint, NotInInterval, InInterval }
import cspfj.problem.{ Variable, Problem, BooleanDomain }
import cspom.constraint.{ GeneralConstraint, CSPOMConstraint }

final class DecomposedAllDifferentGenerator(problem: Problem) extends AbstractGenerator(problem) {
  def generate(constraint: CSPOMConstraint) = {
    require(constraint.isInstanceOf[GeneralConstraint])

    val solverVariables = constraint.scope map cspom2cspfj

    if (solverVariables.exists(_.dom == null)) {
      false
    } else {
      val values = this.values(solverVariables);

      var vis: Map[VariableInterval, VariableInterval] = Map.empty

      for (l <- (0 until values.size)) {
        val lV = values(l);
        for (u <- (l until math.min(values.size, solverVariables.size + l - 1))) {
          val uV = values(u)

          var sum = solverVariables map { v =>
            VariableInterval(v, v.dom.closestGeq(lV), v.dom.closestLeq(uV))
          } filter { vi => vi.lb >= 0 && vi.ub >= 0 } map { vi =>

            vis.get(vi) match {
              case Some(vi) => vi
              case None => {
                vis += vi -> vi
                vi
              }

            }
          }

          if (sum.size > u - l + 1) {
            addConstraint(new SumLeq(u - l + 1, sum map (_.add()) toArray));
          }
        }

      }
      DecomposedAllDifferentGenerator.allDiff += 1;
      true;
    }
  }

  private case class VariableInterval(val variable: Variable, val lb: Int, val ub: Int) {

    var auxVariable: Variable = null;

    def add() = {
      if (auxVariable == null) {
        auxVariable = addVariable(
          "_A" + DecomposedAllDifferentGenerator.allDiff + "_" + variable.name + "_"
            + variable.dom.value(lb) + "_"
            + variable.dom.value(ub), new BooleanDomain());

        addConstraint(new ReifiedConstraint(auxVariable,
          InInterval.indices(variable, lb, ub),
          NotInInterval.indices(variable, lb, ub)));
      }
      auxVariable;
    }

  }

  private def values(variables: Seq[Variable]) = {
    AbstractGenerator.makeDomain(variables.foldLeft(Array[Int]())((acc, v) =>
      acc ++ v.dom.allValues))
  }

}

object DecomposedAllDifferentGenerator {
  var allDiff = 0
}
