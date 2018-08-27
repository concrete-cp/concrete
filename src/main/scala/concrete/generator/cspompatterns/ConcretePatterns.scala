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
        SumGreaterToLesser,
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
        GCC, NValues)

    require(concreteDef.lengthCompare(concreteDef.distinct.size) == 0)

    val concreteImp = Seq(
      //AbsDiff, AbsDiffDomains,
      SubsumedDiff, Square, SumConstants, SumDuplicates, PseudoBool,
      //MergeNotDisj,
      UnaryClause, SumFactors, SumEq, BoolSum, BoolProd, MergeSameCommutative
      , SumSE
      , ClauseSE
      , MinMaxSimplify
      , MinMaxSE,
      AllDiff
    ) //, MergeRelations) //, LexLeq2SAT)

    val improveModel = params.getOrElse("improveModel", true)

    if (improveModel) {
      StandardCompilers() ++ concreteDef ++ StandardCompilers.improve() ++ concreteImp
    } else {
      StandardCompilers() ++ concreteDef
    }

  }

}