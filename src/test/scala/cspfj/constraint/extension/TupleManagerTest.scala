/*
 * Created on 7 mai 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint.extension;

import  org.junit.Assert.assertFalse;
import  org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.AllDifferent;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Variable;

final class TupleManagerTest {

	private var tupleManager: TupleManager = null;

	private var tuple: Array[Int] = null;

	@Before
	def setUp() {

		val dom = Seq( 1, 2, 3, 4, 5 );

		val v1 = new Variable("V1", new BitVectorDomain(dom: _*));
		val v2 = new Variable("V2", new BitVectorDomain(dom: _*));
		val v3 = new Variable("V3", new BitVectorDomain(dom: _*));

		val constraint = new AllDifferent("allDiff", v1, v2, v3);

		tupleManager = new TupleManager(constraint,	constraint.tuple);

		tuple = tupleManager.getTuple;

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
		assertTrue(Arrays.equals(tuple, Array(2, 2, 2)));
	}

}
