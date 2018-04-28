package concrete.heuristic.branch

import java.util.EventObject

import concrete._
import concrete.heuristic.Decision
import concrete.heuristic.value.{RandomValue, ValueHeuristic}

import scala.util.{Random, Try}

object BranchHeuristic {
  def default(params: ParameterManager, rand: Random): Try[BranchHeuristic] = {
    for (heuristic <- BranchHeuristic(params, rand)) yield {
      val randomDiv = params.getOrElse("heuristic.value.randomDiv", 0.0)

      if (randomDiv > 0) {
        new RandomValHeuristic(Seq(heuristic, new ValueHeuristic(new RandomValue(rand))), Seq(1 - randomDiv, randomDiv), rand)
      } else {
        heuristic
      }
    }
  }

  def apply(pm: ParameterManager, rand: Random): Try[BranchHeuristic] = {
    Try(
      pm.classInPackage("heuristic.branch", "concrete.heuristic.value", classOf[ValueHeuristic])
    )
      .flatMap { valueHeuristicClass: Class[_ <: BranchHeuristic] =>
        apply(valueHeuristicClass, pm, rand)
      }
  }

  def apply(branchHeuristicClass: Class[_ <: BranchHeuristic], pm: ParameterManager, rand: Random): Try[BranchHeuristic] = {
    Try(
      branchHeuristicClass.getConstructor().newInstance()
    )
      .recover {
        case _: NoSuchMethodException =>
          branchHeuristicClass.getConstructor(classOf[ParameterManager])
            .newInstance(pm)
      }
      .recover {
        case _: NoSuchMethodException =>
          branchHeuristicClass.getConstructor(classOf[Random])
            .newInstance(rand)
      }
      .recover {
        case _: NoSuchMethodException =>
          branchHeuristicClass.getConstructor(classOf[ParameterManager], classOf[Random])
            .newInstance(pm, rand)
      }
      .recover {
        case _: NoSuchMethodException =>
          branchHeuristicClass.getMethod("apply", classOf[ParameterManager], classOf[Random])
            .invoke(null, pm, rand)
            .asInstanceOf[BranchHeuristic]
      }
  }
}


trait BranchHeuristic {
  def branch(variable: Variable, domain: Domain, ps: ProblemState): Either[Outcome, (ProblemState, Decision, Decision)]

  def compute(solver: MAC, ps: ProblemState): ProblemState

  def shouldRestart: Boolean

  def event[S <: Outcome](e: EventObject, ps: S): S = ps
}