/*
 * Created on 7 mai 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint.extension;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.Constraint;
import cspfj.constraint.extension.TupleManager;
import cspfj.constraint.semantic.AllDifferentConstraint;
import cspfj.problem.Variable;

public class TupleManagerTest {

    private TupleManager tupleManager;

    private int[] tuple;

    @Before
    public void setUp() {

        final int[] dom = new int[] { 1, 2, 3, 4, 5 };

        final Variable v1 = new Variable(dom);
        final Variable v2 = new Variable(dom);
        final Variable v3 = new Variable(dom);

        final Constraint constraint = new AllDifferentConstraint("allDiff",
                new Variable[] { v1, v2, v3 });

        tupleManager = new TupleManager(constraint,
                ((AbstractConstraint) constraint).getTuple());

        tuple = tupleManager.getTuple();

    }

    @Test
    public void testSetPrevTuple() {
        tupleManager.setFirstTuple();
        assertFalse(tupleManager.setPrevTuple(0));

        tupleManager.setFirstTuple();
        assertFalse(tupleManager.setPrevTuple(1));

        tupleManager.setTupleAfter(new int[] { 2, 2, 2 }, 0);
        for (int i = 10; --i >= 0;) {
            assertTrue(tupleManager.setNextTuple(0));
        }
        for (int i = 10; --i >= 0;) {
            assertTrue(tupleManager.setPrevTuple(0));
        }
        assertTrue(Arrays.equals(tuple, new int[] { 2, 2, 2 }));
    }

}
