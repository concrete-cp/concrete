package concrete.heuristic.variable

import java.util.EventObject

import concrete.heuristic.variable
import concrete._

import scala.util.{Random, Try}

object VariableHeuristic {
  def apply(variableHeuristicClass: Class[_ <: VariableHeuristic],
            decisionVariables: Seq[Variable], rand: Random): Try[VariableHeuristic] = {

    Try {
      variableHeuristicClass.getConstructor(classOf[Seq[Variable]])
        .newInstance(decisionVariables)
    }
      .recover {
        case _: NoSuchMethodException =>
          variableHeuristicClass.getConstructor(classOf[Seq[Variable]], classOf[Random])
            .newInstance(decisionVariables, rand)
      }

  }


  def default: Seq[Class[_ <: VariableHeuristic]] = {
    Seq(classOf[variable.WDegOnDom], classOf[variable.RandomVar])
  }

  def default(params: ParameterManager, decisionVariables: Seq[Variable], rand: Random): Try[Seq[VariableHeuristic]] = {

    for {
      vh: Seq[VariableHeuristic] <-
        params.getRaw("heuristic.variable")
          .map {
            case h: Seq[_] => instantiate(h, decisionVariables, rand)
            case s: String => instantiate(s.split(","), decisionVariables, rand)
          }
          .getOrElse(instantiate(default, decisionVariables, rand))
    } yield {
      val randomDiv = params.getOrElse("heuristic.variable.randomDiv", 0.0)

      if (randomDiv > 0) {
        new RandomDiv(vh.flatMap(_.pool).distinct, randomDiv, rand) +: vh
      } else {
        vh
      }
    }
  }

  def instantiate(seq: Seq[Any], decisionVariables: Seq[Variable], rand: Random): Try[Seq[VariableHeuristic]] =
    Try {
      seq.map {
        case h: VariableHeuristic => h
        case s: Class[VariableHeuristic] => VariableHeuristic(s, decisionVariables, rand).get
        case s: String =>
          VariableHeuristic(
            ParameterManager.classInPackage(s, "concrete.heuristic.variable").asInstanceOf[Class[VariableHeuristic]],
            decisionVariables, rand).get
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
