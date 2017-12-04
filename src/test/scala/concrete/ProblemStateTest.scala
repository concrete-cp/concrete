package concrete

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.constraint.semantic.ClauseConstraint

/**
 * @author vion
 */
class ProblemStateTest extends FlatSpec with Matchers {
  "entailment manager" should "compute entailed constraint" in {
    val v1 = new Variable("v1", BooleanDomain())
    val v2 = new Variable("v2", BooleanDomain())

    val c = new ClauseConstraint(Array(v1, v2), Array())

    val problem = Problem(v1, v2)
    problem.addConstraint(c)
    val state = problem.initState.toState
    
    assert(!state.entailed.hasInactiveVar(c))
    
    state.activeConstraints(v1) should contain theSameElementsAs Seq(0)
    state.activeConstraints(v2) should contain theSameElementsAs Seq(0)
    
    val ent = state.entail(c)
    assert(ent.entailed.hasInactiveVar(c))
    
    assert(ent.activeConstraints(v1).isEmpty)
    assert(ent.activeConstraints(v2).isEmpty)
    
    

  }

}