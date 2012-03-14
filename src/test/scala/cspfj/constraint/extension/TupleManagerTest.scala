/*
 * Created on 7 mai 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint.extension;

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test
import cspfj.problem.IntDomain
import cspfj.problem.Variable
import cspfj.constraint.semantic.AllDifferent2C

final class TupleManagerTest {

  private var tupleManager: TupleManager = null;

  private var tuple: Array[Int] = null;

  @Before
  def setUp() {

    val dom = Seq(1, 2, 3, 4, 5);

    val v1 = new Variable("V1", new IntDomain(dom));
    val v2 = new Variable("V2", new IntDomain(dom));
    val v3 = new Variable("V3", new IntDomain(dom));

    val constraint = new AllDifferent2C(v1, v2, v3);

    tupleManager = new TupleManager(constraint, constraint.tuple);

    tuple = tupleManager.tuple;

  }

  @Test
  def testSetPrevTuple() {
    tupleManager.setFirstTuple();
    assertFalse(tupleManager.setPrevTuple(0));

    tupleManager.setFirstTuple();
    assertFalse(tupleManager.setPrevTuple(1));

    tupleManager.setTupleAfter(Array(2, 2, 2), 0);
    for (i <- (1 to 10)) {
      assertTrue(tupleManager.setNextTuple(0));
    }
    for (i <- (1 to 10)) {
      assertTrue(tupleManager.setPrevTuple(0));
    }
    assertTrue(tuple.sameElements(Seq(2, 2, 2)));
  }

}
