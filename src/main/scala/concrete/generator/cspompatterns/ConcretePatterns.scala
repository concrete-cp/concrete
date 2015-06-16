package concrete.generator.cspompatterns

import concrete.ParameterManager
import cspom.compiler.StandardCompilers
import cspom.compiler.MergeSame
import cspom.compiler.MergeEq
import cspom.compiler.RemoveUselessEq
import cspom.compiler.SplitEqVec
import cspom.compiler.ConstraintCompiler

object ConcretePatterns {

  def apply(params: ParameterManager): Seq[ConstraintCompiler] = {
    val concreteDef =
      Seq(
        DivToMul, OccurrenceDomains, BoolEq,
        MulDomains, AbsDomains,
        Bool2IntDomains, MulToSum,
        SumDomains,
        NegToCNF, Xor, ReifiedConj, ReifiedClause,
        NeqVec,
        SlidingSum, Regular, SetIn, ConcreteTypes,
        AllDifferent, AllDiffConstant,
        // Clause does not support constants
        SimplClause 
        )

    val concreteImp = Seq(
      AbsDiff, AbsDiffDomains, AllDiff, SubsumedDiff, Square, SumConstants, SumDuplicates, PseudoBool,
      MergeNotDisj, UnaryClause, SumFactors, SumEq, SumNe) //, LexLeq2SAT)

    val improveModel = params.getOrElse("improveModel", false)

    if (improveModel) {
      StandardCompilers() ++ StandardCompilers.improve() ++ concreteDef ++ concreteImp
    } else {
      StandardCompilers() ++ concreteDef
    }

  }

}