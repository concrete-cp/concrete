package concrete.heuristic

import concrete.ProblemState
import scala.util.Random
import concrete.ParameterManager
import concrete.Variable
import concrete.heuristic.variable.RandomVar
import concrete.heuristic.value.RandomBound
import concrete.Problem
import concrete.MAC

object CrossDiscrepancy {
  def apply(pm: ParameterManager, decisionVariables: Array[Variable]): CrossDiscrepancy = {
    val heuristic = CrossHeuristic(pm, decisionVariables)

    val varProb = pm.getOrElse("discrepancy.varProb", .1)
    val valProb = pm.getOrElse("discrepancy.valProb", .1)

    new CrossDiscrepancy(pm, heuristic, varProb, valProb)
  }

}

class CrossDiscrepancy(pm: ParameterManager, heuristic: CrossHeuristic, varProb: Double, valProb: Double) extends Heuristic {

  private val rand = {
    val seed = pm.getOrElse("discrepancy.seed", 0L) + pm.getOrElse("iteration", 0)
    new Random(seed)
  }

  private val variableHeuristic = heuristic.variableHeuristic
  private val valueHeuristic = heuristic.valueHeuristic

  def decisionVariables = variableHeuristic.decisionVariables

  private val randomVar = new RandomVar(pm, variableHeuristic.decisionVariables)
  private val randomVal = new RandomBound(pm)

  def branch(state: ProblemState) = {
    val varH = if (rand.nextDouble() < varProb) {
      randomVar
    } else {
      variableHeuristic
    }
    varH.select(state).map { v =>
      assert(state.dom(v).size > 1, s"$variableHeuristic selected a singleton variable ${v.toString(state)}")

      val valH = if (rand.nextDouble() < valProb) {
        randomVal
      } else {
        valueHeuristic
      }

      valH.branch(v, state.dom(v), state)
    }
  }

  def compute(p: Problem) = heuristic.compute(p)

  def shouldRestart = varProb > 0 || valProb > 0

  def applyListeners(s: MAC): Unit = heuristic.applyListeners(s)

}