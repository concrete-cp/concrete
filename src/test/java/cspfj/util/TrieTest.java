package cspfj.util;

import java.util.Iterator;

import org.junit.Before;
import org.junit.Test;

import scala.collection.JavaConversions;
import static org.junit.Assert.*;

import cspom.extension.Trie;
import cspom.extension.Trie$;

public class TrieTest {

    private Trie ts;

    @Before
    public void setUp() throws Exception {
        ts = Trie$.MODULE$.empty(2);
        ts = ts.$plus(new int[] { 0, 0 });
        ts = ts.$plus(new int[] { 0, 1 });
        ts = ts.$plus(new int[] { 1, 0 });
    }

    @Test
    public void testContainsTuple() {
        assertTrue(ts.contains(new int[] { 0, 1 }));
        assertFalse(ts.contains(new int[] { 1, 1 }));
    }

    @Test
    public void testRemoveTuple() {
        ts = ts.$minus(new int[] { 0, 1 });
        assertFalse(ts.contains(new int[] { 0, 1 }));
        ts = ts.$minus(new int[] { 1, 1 });
    }

    @Test
    public void testIterator() {
        int i = 0;
        for (Iterator<int[]> itr = JavaConversions
                .asJavaIterator(ts.iterator()); itr.hasNext();) {
            itr.next();
            i++;
        }
        assertEquals(i, ts.size());
    }

    @Test
    public void testSize() {
        assertEquals(3, ts.size());
        ts = ts.$minus(new int[] { 0, 0 });
        assertEquals(2, ts.size());
        ts = ts.$plus(new int[] { 1, 1 });
        assertEquals(3, ts.size());
    }
}
