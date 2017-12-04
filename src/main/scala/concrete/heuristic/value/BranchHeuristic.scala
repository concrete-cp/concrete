package concrete
package heuristic.value

import java.util.EventObject

import concrete.heuristic.{Assign, Decision, Remove}

import scala.util.{Random, Try}

object BranchHeuristic {
  def apply(pm: ParameterManager, rand: Random): Try[BranchHeuristic] = {
    Try(
      pm.classInPackage("heuristic.value", "concrete.heuristic.value", classOf[BestValue])
    )
      .flatMap { valueHeuristicClass: Class[_ <: BranchHeuristic] =>
        apply(valueHeuristicClass, pm, rand)
      }
  }

  def apply(valueHeuristicClass: Class[_ <: BranchHeuristic], pm: ParameterManager, rand: Random): Try[BranchHeuristic] = {
    Try(
      valueHeuristicClass.getConstructor().newInstance()
    )
      .recover {
        case _: NoSuchMethodException =>
          valueHeuristicClass.getConstructor(classOf[ParameterManager])
            .newInstance(pm)
      }
      .recover {
        case _: NoSuchMethodException =>
          valueHeuristicClass.getConstructor(classOf[Random])
            .newInstance(rand)
      }
      .recover {
        case _: NoSuchMethodException =>
          valueHeuristicClass.getConstructor(classOf[ParameterManager], classOf[Random])
            .newInstance(pm, rand)
      }
      .recover {
        case _: NoSuchMethodException =>
          valueHeuristicClass.getMethod("apply", classOf[ParameterManager], classOf[Random])
            .invoke(null, pm, rand)
            .asInstanceOf[BranchHeuristic]
      }
  }
}

trait ValueHeuristic extends BranchHeuristic {
  def branch(variable: Variable, domain: Domain, ps: ProblemState): (Decision, Decision) = {
    assignBranch(ps, variable, selectIndex(variable, domain))
  }

  def selectIndex(variable: Variable, domain: Domain): Int

}

trait BranchHeuristic {
  def assignBranch(ps: ProblemState, variable: Variable, selected: Int): (Decision, Decision) = {
    (Assign(variable, selected), Remove(variable, selected))
  }

  def branch(variable: Variable, domain: Domain, ps: ProblemState): (Decision, Decision)

  def compute(solver: MAC, ps: ProblemState): ProblemState

  def shouldRestart: Boolean

  def event[S <: Outcome](e: EventObject, ps: S): S = ps
}