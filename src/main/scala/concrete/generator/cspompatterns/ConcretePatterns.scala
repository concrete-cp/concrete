package concrete.generator.cspompatterns

import concrete.ParameterManager
import cspom.compiler.StandardCompilers
import cspom.compiler.MergeSame
import cspom.compiler.MergeEq
import cspom.compiler.RemoveUselessEq
import cspom.compiler.SplitEqVec
import cspom.compiler.ConstraintCompiler
import cspom.compiler.CSPOMTypes

object ConcretePatterns {

  def apply(params: ParameterManager): Seq[ConstraintCompiler] = {
    val concreteDef =
      Seq(
        DivToMul, OccurrenceDomains, BoolEq,
        MulDomains, AbsDomains,
        MulToSum,
        SumDomains, NeDomains,
        NegToCNF, Xor, ReifiedConj, ReifiedClause,
        NeqVec,
        SlidingSum, Regular, SetIn, CSPOMTypes,
        AllDifferent, AllDiffConstant,
        // Clause does not support constants
        SimplClause,
    Bool2IntDomains)

    val concreteImp = Seq(
      //AbsDiff, AbsDiffDomains,
      AllDiff, SubsumedDiff, Square, SumConstants, SumDuplicates, PseudoBool,
      MergeNotDisj, UnaryClause, SumFactors, SumEq, SumFalse) //, LexLeq2SAT)

    val improveModel = params.getOrElse("improveModel", true)

    if (improveModel) {
      StandardCompilers() ++ StandardCompilers.improve() ++ concreteDef ++ concreteImp
    } else {
      StandardCompilers() ++ concreteDef
    }

  }

}