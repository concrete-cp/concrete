package cspfj.constraint.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import org.junit.Before;
import org.junit.Test;

public class TupleSetTest {

    private TupleSet ts;

    @Before
    public void setUp() throws Exception {
        ts = new TupleSet(false);
        ts.add(new int[] { 0, 0 });
        ts.add(new int[] { 0, 1 });
        ts.add(new int[] { 1, 0 });
    }

    @Test
    public void testContainsTuple() {
        assertTrue(ts.containsTuple(new int[] { 0, 1 }));
        assertFalse(ts.containsTuple(new int[] { 1, 1 }));
    }

    @Test
    public void testRemoveTuple() {
        ts.removeTuple(new int[] { 0, 1 });
        assertFalse(ts.containsTuple(new int[] { 0, 1 }));
        ts.removeTuple(new int[] { 1, 1 });
    }

    @Test
    public void testIterator() {
        int i = 0;
        for (Iterator<int[]> itr = ts.iterator(); itr.hasNext();) {
            itr.next();
            i++;
        }
        assertEquals(i, ts.size());
    }

    @Test
    public void testSize() {
        assertEquals(3, ts.size());
        ts.removeTuple(new int[] { 0, 0 });
        assertEquals(2, ts.size());
        ts.add(new int[] { 1, 1 });
        assertEquals(3, ts.size());
    }
}
