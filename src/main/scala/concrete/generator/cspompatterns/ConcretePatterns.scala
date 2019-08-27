package concrete
package generator
package cspompatterns

import cspom.compiler.{CSPOMTypes, Compiler, StandardCompilers}

object ConcretePatterns {

  def apply(params: ParameterManager): Seq[Compiler] = {
    val concreteDef =
      Seq(
        SumGreaterToLesser,
        Occurrence, AtLeastDomains, AtMostDomains, BoolEq,
        MulDomains, AbsDomains, AbsDiffDomains, Reversed, ClauseDomains,
        MulToSum,
        SumDomains,
        NegToCNF, ReifiedXor, ReifiedConj, ReifiedClause,
        NeqVec,
        SlidingSum, Regular, SetIn, CSPOMTypes,
        AllDiffConstant, Pow,
        // Clause does not support constants
        SimplClause, UnaryClause, PBConstants,
        DivDomains, ModDomains,
        Knapsack, Lex, NoOverlap, MinDomains, MaxDomains, DiffNWCumulative,
        GCC, NValues)

    require(concreteDef.lengthCompare(concreteDef.distinct.size) == 0)

    val concreteImp = Seq(
      SumDuplicates, ClauseDuplicates,
      //AbsDiff, AbsDiffDomains,
      SubsumedDiff, Square, SumConstants,  PseudoBool,
      MergeNotDisj,
      SumFactors, SumEq, BoolSum, BoolProd, MergeSameCommutative,
      ACCSE_Sum,
      ACCSE_Clause,
      MinMaxSimplify,
      ACCSE_MinMax,
      MergeSameSum,
      AllDiff,
    )

    val improveModel = params.getOrElse("improveModel", true)

    if (improveModel) {
      StandardCompilers() ++ concreteDef ++ StandardCompilers.improve() ++ concreteImp
    } else {
      StandardCompilers() ++ concreteDef
    }

  }

}