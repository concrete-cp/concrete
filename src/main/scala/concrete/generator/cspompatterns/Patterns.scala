package concrete.generator.cspompatterns

import cspom.compiler.ConstraintTyper
import cspom.compiler.ConstraintSignature
import cspom.variable.CSPOMInt

object Patterns {
  def apply() =
    Seq(AbsDiff, SubsumedDiff, AllDiff, DiffGe, MergeDisj, MergeEq, MergeSame, NeqVec, RemoveAnd)

}