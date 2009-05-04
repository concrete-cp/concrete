package cspfj.constraint.extension;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import cspfj.problem.Variable;

public class MatrixManagerDynamicTest {

    MatrixManagerDynamic mmd;
    int[][] content;

    @Before
    public void setUp() throws Exception {
        final TupleSet ta = new TupleSet(false);
        ta.set(new int[] { 0, 0 }, true);
        ta.set(new int[] { 1, 1 }, true);
        ta.set(new int[] { 2, 2 }, true);

        final int[] dom = new int[] { 0, 1, 2 };

        final Variable[] scope = { new Variable(dom), new Variable(dom) };

        mmd = new MatrixManagerDynamic(scope, ta, false);
        final Iterator<int[]> itr = mmd.iterator();
        content = new int[][] { itr.next(), itr.next(), itr.next() };
    }

    @Test
    public void testRestore() {
        final Iterator<int[]> itr2 = mmd.iterator();
        mmd.setLevel(1);
        itr2.next();
        itr2.next();
        itr2.remove();
        itr2.next();
        itr2.remove();

        {
            final Iterator<int[]> itr = mmd.iterator();
            assertArrayEquals(content[0], itr.next());
            assertFalse(itr.hasNext());
        }

        mmd.restore(0);

        {
            final Set<int[]> set = new HashSet<int[]>(Arrays.asList(content));
            final Iterator<int[]> itr1 = mmd.iterator();
            assertTrue(set.remove(itr1.next()));
            assertTrue(set.remove(itr1.next()));
            assertTrue(set.remove(itr1.next()));
            assertFalse(itr1.hasNext());
            assertTrue(set.isEmpty());
        }
    }

    @Test
    public void testIterator() {
        {
            final Iterator<int[]> itr1 = mmd.iterator();
            assertTrue(itr1.hasNext());
            assertArrayEquals(content[0], itr1.next());
            assertTrue(itr1.hasNext());
            assertArrayEquals(content[1], itr1.next());
            assertTrue(itr1.hasNext());
            assertArrayEquals(content[2], itr1.next());
            assertFalse(itr1.hasNext());
        }
        {
            final Iterator<int[]> itr2 = mmd.iterator();
            assertArrayEquals(content[0], itr2.next());
            itr2.remove();
            assertTrue(itr2.hasNext());
            assertArrayEquals(content[1], itr2.next());
            assertArrayEquals(content[2], itr2.next());
            assertFalse(itr2.hasNext());
        }
        {
            final Iterator<int[]> itr2 = mmd.iterator();
            assertArrayEquals(content[1], itr2.next());
            assertArrayEquals(content[2], itr2.next());
            itr2.remove();
            assertFalse(itr2.hasNext());

        }
        {
            final Iterator<int[]> itr2 = mmd.iterator();
            assertArrayEquals(content[1], itr2.next());
            itr2.remove();
            assertFalse(itr2.hasNext());
        }
        {
            final Iterator<int[]> itr2 = mmd.iterator();
            assertFalse(itr2.hasNext());
        }
    }

}
