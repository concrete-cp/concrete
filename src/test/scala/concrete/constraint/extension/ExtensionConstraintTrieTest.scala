package concrete.constraint.extension;

import org.scalatest.Finders
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import concrete.IntDomain
import concrete.Problem
import concrete.ProblemState
import concrete.Variable
import concrete.constraint.AdviseCount

final class ExtensionConstraintTrieTest extends FlatSpec with Matchers {

  val ta = new TupleTrieSet(new MDDRelation(), false);
  ta.set(Array(0, 0), true);
  ta.set(Array(1, 1), true);
  ta.set(Array(2, 2), true);

  val v0 = new Variable("V0", IntDomain(0 to 1))
  val v1 = new Variable("V1", IntDomain(0 to 2))

  val mmd = new ReduceableExt(Array(v0, v1), ta.reduceable);
  mmd.register(new AdviseCount())

  //println(content map (_.toSeq) mkString (", "))

  "ReduceableExt" should "filter" in {
    val problem = Problem(v0, v1)
    problem.addConstraint(mmd)
    val state = problem.initState.toState
    mmd.adviseAll(state)

    val mod = mmd.revise(state).toState

    mod.dom(v0) should be theSameInstanceAs (v0.initDomain)
    mod.dom(v1) should not be theSameInstanceAs(v1.initDomain)
    mod(mmd) should have size 2

  }
}

