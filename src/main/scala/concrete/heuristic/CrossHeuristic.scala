package concrete
package heuristic

import java.util.EventObject

import concrete.heuristic.value.{BestValue, BranchHeuristic}
import concrete.heuristic.variable._

object CrossHeuristic {
  def apply(params: ParameterManager, decisionVariables: Seq[Variable]): CrossHeuristic = {
    val varH = defaultVar(params, decisionVariables)
    val valH = defaultVal(params)

    CrossHeuristic(varH, valH)
  }

  def defaultVar(params: ParameterManager, decisionVariables: Seq[Variable]): VariableHeuristic = {

    val tieBreaker = params.getRaw("heuristic.variable.tieBreaker").map {
      case h: VariableHeuristic => h
      case "rand" => new RandomVar(Seq(), params)
      case "lex" => new LexVar(Seq())
    }.getOrElse(new RandomVar(Seq(), params))


    val variableHeuristicClass: Class[_ <: VariableHeuristic] =
      params.classInPackage("heuristic.variable", "concrete.heuristic.variable", classOf[variable.WDegOnDom])

    val vh: VariableHeuristic =
      if (classOf[ScoredVariableHeuristic].isAssignableFrom(variableHeuristicClass)) {

        variableHeuristicClass.getConstructor(classOf[Seq[Variable]], classOf[VariableHeuristic], classOf[Boolean])
          .newInstance(decisionVariables, tieBreaker, Boolean.box(false))

      } else {
        try {
          variableHeuristicClass.getConstructor(classOf[Seq[Variable]], classOf[ParameterManager])
            .newInstance(decisionVariables, params)
        } catch {
          case _: NoSuchMethodException =>
            variableHeuristicClass.getConstructor(classOf[Seq[Variable]]).newInstance(decisionVariables)
        }
      }


    val randomDiv = params.getOrElse("heuristic.variable.randomDiv", 0.2)

    if (randomDiv > 0) {
      new RandomDiv(params, vh, randomDiv)
    } else {
      vh
    }
  }

  def defaultVal(params: ParameterManager): BranchHeuristic = {
    val valueHeuristicClass: Class[_ <: BranchHeuristic] =
      params.classInPackage("heuristic.value", "concrete.heuristic.value", classOf[BestValue])

    valueHeuristicClass
      .getConstructor(classOf[ParameterManager])
      .newInstance(params)
  }

  def apply(varH: VariableHeuristic, valH: BranchHeuristic): CrossHeuristic =
    CrossHeuristic(varH, valH, varH.shouldRestart || valH.shouldRestart)
}

final case class CrossHeuristic(
                                 variableHeuristic: VariableHeuristic,
                                 valueHeuristic: BranchHeuristic,
                                 shouldRestart: Boolean) extends Heuristic {

  def branch(state: ProblemState, candidates: Seq[Variable]): Option[Branch] = {
    variableHeuristic.select(state, candidates).map { v =>
      assert(state.dom(v).size > 1, s"$variableHeuristic selected a singleton variable ${v.toString(state)}")
      valueHeuristic.branch(v, state.dom(v), state)
    }
  }

  def compute(solver: MAC, state: ProblemState): ProblemState = {
    // logger.fine("Initializing heuristics");
    val ps = variableHeuristic.compute(solver, state)
    valueHeuristic.compute(solver, ps)
  }

  override def toString = s"Crossed ($variableHeuristic, $valueHeuristic)"

  def decisionVariables: Seq[Variable] = variableHeuristic.pool

  def event(event: EventObject): Unit = {
    variableHeuristic.event(event)
    valueHeuristic.event(event)
  }
}
