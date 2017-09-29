package concrete.heuristic

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.ParameterManager
import concrete.Variable
import concrete.IntDomain
import concrete.Problem
import concrete.heuristic.variable.{Dom, RandomVar}

/**
 * @author vion
 */
class ScoredVariableHeuristicTest extends FlatSpec with Matchers {
  "Scored Variable Heuristic" should "select smallest variable with random tie breaking" in {
    val pm = new ParameterManager()
    //    pm("variableHeuristic.randomBreak") = false

    val variables = Array.tabulate(10)(n => new Variable(n.toString, IntDomain(0 to 10)))

    val h = new Dom(variables, new RandomVar(variables, pm))

    val problem = new Problem(variables)
    val state = h.compute(null, problem.initState.toState)

    val select = List.fill(100)(h.select(state, variables)).distinct

    select.size should be > 1

  }
}