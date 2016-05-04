package concrete.generator.cspompatterns

import concrete.ParameterManager
import cspom.compiler.CSPOMTypes
import cspom.compiler.ConstraintCompiler
import cspom.compiler.StandardCompilers

object ConcretePatterns {

  def apply(params: ParameterManager): Seq[ConstraintCompiler] = {
    val concreteDef =
      Seq(
        OccurrenceDomains, BoolEq,
        MulDomains, AbsDomains,
        MulToSum,
        SumDomains, NeDomains,
        NegToCNF, Xor, ReifiedConj, ReifiedClause,
        NeqVec,
        SlidingSum, Regular, SetIn, CSPOMTypes,
        AllDifferent, AllDiffConstant,
        // Clause does not support constants
        SimplClause, PBConstants,
        Bool2IntDomains, DivDomains, ModDomains,
        Knapsack)

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