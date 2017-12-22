package concrete
package generator
package cspompatterns

import cspom.compiler.CSPOMTypes
import cspom.compiler.Compiler
import cspom.compiler.StandardCompilers

object ConcretePatterns {

  def apply(params: ParameterManager): Seq[Compiler] = {
    val concreteDef =
      Seq(
        Occurrence, AtLeastDomains, AtMostDomains, BoolEq,
        MulDomains, AbsDomains, AbsDiffDomains, Reversed, ClauseDomains,
        MulToSum,
        SumDomains,
        NegToCNF, ReifiedXor, ReifiedConj, ReifiedClause,
        NeqVec,
        SlidingSum, Regular, SetIn, CSPOMTypes,
        AllDiffConstant, PowDomains, Pow,
        // Clause does not support constants
        SimplClause, PBConstants,
        DivDomains, ModDomains,
        Knapsack, Lex, NoOverlap, MinDomains, MaxDomains, DiffNWCumulative,
        GCC)

    require(concreteDef.distinct.size == concreteDef.size)

    val concreteImp = Seq(
      //AbsDiff, AbsDiffDomains,
      AllDiff, SubsumedDiff, Square, SumConstants, SumDuplicates, PseudoBool,
      //MergeNotDisj,
      UnaryClause, SumFactors, SumEq, BoolSum, BoolProd, MergeSameCommutative
      , SumSE, ClauseSE, MinMaxSE
    ) //, MergeRelations) //, LexLeq2SAT)

    val improveModel = params.getOrElse("improveModel", true)

    if (improveModel) {
      StandardCompilers() ++ StandardCompilers.improve() ++ concreteDef ++ concreteImp
    } else {
      StandardCompilers() ++ concreteDef
    }

  }

}