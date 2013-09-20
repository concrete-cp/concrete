package concrete.generator.cspompatterns

import cspom.constraint.FunctionalConstraint
import cspom.constraint.CSPOMConstraint
import cspom.CSPOM
import scala.collection.mutable.Queue
import scala.util.control.Breaks._
import cspom.compiler.ConstraintCompiler

/**
 * Transforms x = a \/ b, x \/ c \/ ... into a \/ b \/ c \/ ...
 */
final class NeqVec(
  private val problem: CSPOM,
  private val constraints: Queue[CSPOMConstraint]) extends ConstraintCompiler {

  private def isNevec(c: CSPOMConstraint) = (c.description == "ne" || c.description == "nevec") && c.isInstanceOf[FunctionalConstraint]

  override def compileFunctional(fc: FunctionalConstraint): Boolean = {
    isNevec(fc) && ((fc.result.constraints - fc).toSeq match {
      case Seq(orConstraint: GeneralConstraint) if orConstraint.description == "or" =>
        val orVariables = orConstraint.scope
        val neConstraints = orVariables.flatMap(_.constraints).filter(_ ne orConstraint)

        if (neConstraints.forall(isNevec)) {

          neConstraints.foreach(problem.removeConstraint)
          problem.removeConstraint(orConstraint)
          orVariables.foreach(problem.removeVariable)

          val (x, y) = neConstraints.flatMap { c =>
            val args = c.asInstanceOf[FunctionalConstraint].arguments
            args.splitAt(args.size / 2).zipped.toSeq
          }.unzip

          problem.addConstraint(new GeneralConstraint("nevec", (x ++ y): _*))
          true
        } else false
      //        problem.removeConstraint
      //          .exists(orVariable =>
      //            (orVariable ne fc.result) && (
      //              (orVariable.constraints - orConstraint).toSeq match {
      //                case Seq(neConstraint: FunctionalConstraint) if isNevec(neConstraint) => {
      //                  problem.removeConstraint(fc)
      //                  problem.removeConstraint(neConstraint)
      //                  problem.removeConstraint(orConstraint)
      //                  problem.removeVariable(orVariable)
      //
      //                  val (x1, y1) = fc.arguments.splitAt(fc.arguments.size / 2)
      //                  val (x2, y2) = neConstraint.arguments.splitAt(neConstraint.arguments.size / 2)
      //
      //                  val orScope = orConstraint.scope.filter(_ ne orVariable)
      //
      //                  if (orScope.size > 1) {
      //                    problem.addConstraint(new FunctionalConstraint(fc.result, "nevec", ((x1 ++ x2) ++ (y1 ++ y2)): _*))
      //                    problem.addConstraint(new GeneralConstraint("or", orScope: _*))
      //                  } else {
      //                    problem.addConstraint(new GeneralConstraint("nevec", ((x1 ++ x2) ++ (y1 ++ y2)): _*))
      //                  }
      //
      //                  for (v <- x1 ++ y1 ++ x2 ++ y2; c <- v.constraints) {
      //                    constraints.enqueue(c)
      //                  }
      //                  true
      //                }
      //                case _ => false
      //              }))
      case _ => false
    })
  }

}
