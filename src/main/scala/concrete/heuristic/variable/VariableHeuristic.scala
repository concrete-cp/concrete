package concrete.heuristic.variable

import java.util.EventObject

import concrete.heuristic.variable
import concrete._

import scala.util.{Random, Try}

object VariableHeuristic {
  def default(params: ParameterManager, decisionVariables: Seq[Variable], rand: Random): Try[Seq[VariableHeuristic]] = {

    //for {
    //  vh: Seq[VariableHeuristic] <-
    params.getRaw("heuristic.variable")
      .map {
        case h: Seq[_] => instantiate(params, h, decisionVariables, rand)
        case s: String => instantiate(params, s.split(",").toSeq, decisionVariables, rand)
      }
      .getOrElse(instantiate(params, default, decisionVariables, rand))
    //}

    //      yield {
    //      val randomDiv = params.getOrElse("heuristic.variable.randomDiv", 0.0)
    //
    //      if (randomDiv > 0) {
    //        new RandomDiv(vh.flatMap(_.pool).distinct, randomDiv, rand) +: vh
    //      } else {
    //        vh
    //      }
    //    }
  }

  def default: Seq[Class[_ <: VariableHeuristic]] = {
    Seq(classOf[variable.LastConflict], classOf[variable.WDegOnDom], classOf[variable.RandomVar])
  }

  def instantiate(pm: ParameterManager, seq: Seq[Any], decisionVariables: Seq[Variable], rand: Random): Try[Seq[VariableHeuristic]] =
    Try {
      seq.map {
        case h: VariableHeuristic => h
        case s: Class[VariableHeuristic] => VariableHeuristic(pm, s, decisionVariables, rand).get
        case s: String =>
          VariableHeuristic(
            pm,
            ParameterManager.classInPackage(s, "concrete.heuristic.variable").asInstanceOf[Class[VariableHeuristic]],
            decisionVariables, rand).get
      }
    }

  def apply(pm: ParameterManager, variableHeuristicClass: Class[_ <: VariableHeuristic],
            decisionVariables: Seq[Variable], rand: Random): Try[VariableHeuristic] = {

    Try {
      variableHeuristicClass.getConstructor(classOf[Seq[Variable]])
        .newInstance(decisionVariables)
    }
      .recover {
        case _: NoSuchMethodException =>
          variableHeuristicClass.getConstructor(classOf[ParameterManager], classOf[Seq[Variable]])
            .newInstance(pm, decisionVariables)
      }
      .recover {
        case _: NoSuchMethodException =>
          variableHeuristicClass.getConstructor(classOf[Seq[Variable]], classOf[Random])
            .newInstance(decisionVariables, rand)
      }
      .recover {
        case _: NoSuchMethodException =>
          variableHeuristicClass.getConstructor(classOf[ParameterManager], classOf[Seq[Variable]], classOf[Random])
            .newInstance(pm, decisionVariables, rand)
      }
  }
}

abstract class VariableHeuristic {
  def pool: Seq[Variable]

  def select(state: ProblemState, candidates: Seq[Variable]): Seq[Variable]

  def compute(s: MAC, ps: ProblemState): ProblemState

  def shouldRestart: Boolean

  def event[S <: Outcome](e: EventObject, state: S): S
}
