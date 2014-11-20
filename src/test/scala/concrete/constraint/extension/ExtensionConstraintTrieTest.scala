package concrete.constraint.extension;

import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import concrete.IntDomain
import concrete.Variable
import concrete.Revised
import concrete.constraint.AdviseCount
import org.scalatest.FlatSpec
import org.scalatest.Matchers

final class ExtensionConstraintTrieTest extends FlatSpec with Matchers {

  val ta = new TupleTrieSet(new MDDRelation(), false);
  ta.set(Array(0, 0), true);
  ta.set(Array(1, 1), true);
  ta.set(Array(2, 2), true);

  val scope = Array(
    new Variable("V0", IntDomain(0 to 1)),
    new Variable("V1", IntDomain(0 to 2)))

  val mmd = new ReduceableExt(scope, ta.reduceable);
  mmd.register(new AdviseCount())

  //println(content map (_.toSeq) mkString (", "))

  "ReduceableExt" should "filter" in {
    val state = mmd.initState
    mmd.adviseAll(scope.map(_.initDomain))

    val Revised(mod, _, newState) = mmd.revise(scope.map(_.initDomain), state)

    mod(0) should be theSameInstanceAs (scope(0).initDomain)
    mod(1) should not be theSameInstanceAs(scope(1).initDomain)
    newState should have size 2

  }
}

