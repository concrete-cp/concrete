package concrete.generator.cspompatterns

import cspom.compiler.ConstraintTyper
import cspom.compiler.ConstraintSignature
import cspom.variable.CSPOMInt

object Patterns {
  def apply() =
    Seq(AbsDiff, SubsumedDiff, AllDiff, DiffGe, MergeDisj, MergeEq, MergeSame, NeqVec, RemoveAnd,
      new ConstraintTyper(knownConstraints))

  private def knownConstraints = Seq(
    ConstraintSignature(CSPOMInt, "add", CSPOMInt, CSPOMInt))
}