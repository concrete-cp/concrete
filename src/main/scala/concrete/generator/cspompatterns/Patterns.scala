package concrete.generator.cspompatterns

import cspom.compiler.ConstraintTyper
import cspom.compiler.ConstraintSignature
import cspom.variable.CSPOMInt

object Patterns {
  def apply() =
    Seq(AbsDiff,
      SubsumedDiff, AllDiff, RemoveUselessEq, DiffGe,
      UnaryOr, MergeDisj, MergeNotDisj, MergeEq, 
      NegToCNF, ReifiedDisj, ReifiedConj,
      SimplDisj, MergeSame,
      NeqVec, RemoveAnd, SplitAllEq)

}