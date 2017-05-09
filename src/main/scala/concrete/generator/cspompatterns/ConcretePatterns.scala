package concrete
package generator
package cspompatterns

import cspom.compiler.CSPOMTypes
import cspom.compiler.ConstraintCompiler
import cspom.compiler.StandardCompilers

object ConcretePatterns {

  def apply(params: ParameterManager): Seq[ConstraintCompiler] = {
    val concreteDef =
      Seq(
        Occurrence, AtLeastDomains, AtMostDomains, BoolEq,
        MulDomains, AbsDomains, AbsDiffDomains, Reversed, ClauseDomains,
        MulToSum,
        SumDomains,
        NegToCNF, ReifiedXor, ReifiedConj, ReifiedClause,
        NeqVec,
        SlidingSum, Regular, SetIn, CSPOMTypes,
        AllDiffConstant,
        // Clause does not support constants
        SimplClause, PBConstants,
        Bool2IntDomains, DivDomains, ModDomains,
        Knapsack, Lex, NoOverlap, MinDomains, MaxDomains, DiffNWCumulative)

    require(concreteDef.distinct.size == concreteDef.size)

    val concreteImp = Seq(
      //AbsDiff, AbsDiffDomains,
      AllDiff, SubsumedDiff, Square, SumConstants, SumDuplicates, PseudoBool,
      MergeNotDisj, UnaryClause, SumFactors, SumEq, BoolSum) //, MergeRelations) //, LexLeq2SAT)

    val improveModel = params.getOrElse("improveModel", true)

    if (improveModel) {
      StandardCompilers() ++ StandardCompilers.improve() ++ concreteDef ++ concreteImp
    } else {
      StandardCompilers() ++ concreteDef
    }

  }

}