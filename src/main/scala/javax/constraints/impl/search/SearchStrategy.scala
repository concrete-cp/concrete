package javax.constraints.impl.search

import concrete.heuristic.Heuristic
import concrete.Problem
import concrete.Pair
import javax.constraints.VarSelectorType
import concrete.ParameterManager
import concrete.heuristic.LexVar
import concrete.heuristic.Dom
import concrete.heuristic.DDegOnDom
import concrete.heuristic.WDegOnDom
import concrete.heuristic.WDeg
import javax.constraints.ValueSelectorType
import concrete.heuristic.Lexico
import concrete.heuristic.RevLexico
import concrete.heuristic.MedValue
import concrete.MAC
import concrete.heuristic.CrossHeuristic

class SearchStrategy(solver: Solver) extends AbstractSearchStrategy(solver) {
  override def setVarSelectorType(varSelectorType: VarSelectorType) {
    ParameterManager("heuristic.variable") = varSelectorType match {
      case VarSelectorType.INPUT_ORDER => classOf[LexVar]
      case VarSelectorType.MIN_DOMAIN | VarSelectorType.MIN_DOMAIN_RANDOM => classOf[Dom]
      case VarSelectorType.MIN_DOMAIN_MIN_VALUE =>
        ParameterManager("variableHeuristic.randomBreak") = false
        classOf[Dom]

      case VarSelectorType.MIN_DOMAIN_OVER_DEGREE => classOf[DDegOnDom]
      case VarSelectorType.MIN_DOMAIN_OVER_WEIGHTED_DEGREE => classOf[WDegOnDom]
      case VarSelectorType.MAX_WEIGHTED_DEGREE => classOf[WDeg]
      case e: VarSelectorType => throw new IllegalArgumentException(s"Unknown variable selector type $e")
    }
    solver.concreteSolver.asInstanceOf[MAC].heuristic = new CrossHeuristic()
  }

  override def setValueSelectorType(valueSelectorType: ValueSelectorType) {
    ParameterManager("heuristic.value") = valueSelectorType match {
      case ValueSelectorType.MIN => classOf[Lexico]
      case ValueSelectorType.MAX => classOf[RevLexico]
      case ValueSelectorType.MEDIAN => classOf[MedValue]
      case _ => throw new IllegalArgumentException("Unknown value selector type")
    }
    solver.concreteSolver.asInstanceOf[MAC].heuristic = new CrossHeuristic()
  }
}