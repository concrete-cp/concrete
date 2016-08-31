package concrete.constraint.semantic

import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers

import concrete.IntDomain
import concrete.Problem
import concrete.Singleton
import concrete.Variable
import concrete.constraint.AdviseCount

/**
 * @author vion
 */
class DivTest extends FlatSpec with Matchers with Inspectors {
  "DivAC" should "comply with MiniZinc specifications" in {

    forAll(Seq((7, 4, 1), (-7, 4, -1), (7, -4, -1), (-7, -4, 1))) {
      case (xv, yv, zv) =>

        val x = new Variable("x", Singleton(xv))
        val y = new Variable("y", Singleton(yv))
        val z = new Variable("z", IntDomain.ofInterval(-1000, 1000))

        val problem = Problem(x, y, z)
        val constraint = new DivAC(x, y, z)
        problem.addConstraint(constraint)
        val state = problem.initState.toState
        val advisor = new AdviseCount
        constraint.register(advisor)
        constraint.adviseAll(state)
        val mod = constraint.revise(state)

        mod.dom(z).view should contain theSameElementsAs Seq(zv)
    }
  }

  // TODO
    "DivBC" should "comply with MiniZinc specifications" in {
  
      forAll(Seq((7, 4, 1), (-7, 4, -1), (7, -4, -1), (-7, -4, 1))) {
        case (xv, yv, zv) =>
          val x = new Variable("x", Singleton(xv))
          val y = new Variable("y", Singleton(yv))
          val z = new Variable("z", IntDomain.ofInterval(-1000, 1000))
  
          val problem = Problem(x, y, z)
          val constraint = new DivBC(x, y, z)
          problem.addConstraint(constraint)
          val state = problem.initState.toState
          constraint.adviseAll(state)
          val mod = constraint.revise(state)
  
          mod.dom(z).view should contain theSameElementsAs Seq(zv)
      }
    }
}