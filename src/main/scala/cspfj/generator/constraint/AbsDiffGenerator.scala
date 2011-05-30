package cspfj.generator.constraint;

import com.google.common.primitives.Ints
import cspfj.constraint.semantic.AbsDiff
import cspfj.exception.FailedGenerationException
import cspfj.problem.BitVectorDomain
import cspfj.problem.Problem
import cspfj.problem.Variable
import cspom.constraint.CSPOMConstraint
import cspom.variable.CSPOMDomain
import scala.collection.immutable.TreeSet

final class AbsDiffGenerator(problem: Problem) extends AbstractGenerator(problem) {

  override def generate(constraint: CSPOMConstraint) = {
    require(constraint.description == "absdiff")

    val Seq(result, v0, v1) = constraint.scope.map(getSolverVariable)

    if (Seq(result, v0, v1).count(_.getDomain == null) > 1) {
      false
    } else {
      if (result.getDomain == null) {

        val values = TreeSet[Int]() ++ (for (i <- v0.getDomain.allValues; j <- v1.getDomain.allValues) yield math.abs(i - j))

        result.setDomain(new BitVectorDomain(values.toSeq: _*));

      } else if (v0.getDomain == null) {

        v0.setDomain(new BitVectorDomain(generateValues(result, v1): _*));

      } else if (v1.getDomain == null) {

        v1.setDomain(new BitVectorDomain(generateValues(result, v0): _*));

      }
      addConstraint(new AbsDiff(result, v0, v1));
      true;
    }

  }

  private def generateValues(result: Variable, variable: Variable) = {
    (TreeSet[Int]() ++ (for (i <- result.getDomain.allValues; j <- variable.getDomain.allValues) yield Seq(i + j, i - j)).flatten).toSeq
  }

}
