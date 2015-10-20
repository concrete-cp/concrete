package concrete.heuristic

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import concrete.ParameterManager
import cspom.variable.IntVariable
import concrete.Variable
import concrete.IntDomain
import concrete.Problem

/**
 * @author vion
 */
class ScoredVariableHeuristicTest extends FlatSpec with Matchers {
  "Scored Variable Heuristic" should "select smallest variable with random tie breaking" in {
    val pm = new ParameterManager()
    //    pm("variableHeuristic.randomBreak") = false

    val variables = Array.tabulate(10)(n => new Variable(n.toString, IntDomain(0 to 10)))

    val h = new Dom(pm, variables)

    val problem = new Problem(variables)
    val state = problem.initState.toState

    val select = List.fill(100)(h.select( state).get).distinct

    select.size should be > 1

  }
}