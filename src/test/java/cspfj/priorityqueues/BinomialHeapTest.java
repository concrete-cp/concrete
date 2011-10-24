package cspfj.priorityqueues;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Queue;
import java.util.Random;

import org.junit.Test;

public final class BinomialHeapTest {

    private static int id = 0;

    private static final Random RANDOM = new Random();

    private static final IdInteger[] INTS;

    static {
        INTS = new IdInteger[100000];
        for (int i = INTS.length; --i >= 0;) {
            INTS[i] = new IdInteger(RANDOM.nextInt(5000000));
        }
    }

    @Test
    public void test() {
        final Queue<IdInteger> maximier = new BinomialHeap<IdInteger>(
                new Key<IdInteger>() {

                    @Override
                    public double getKey(final IdInteger object) {
                        return object.value;
                    }

                });

        assertEquals(maximier.size(), 0);
        assertTrue(maximier.isEmpty());

        for (IdInteger i : INTS) {
            maximier.offer(i);
        }
        assertEquals(maximier.size(), INTS.length);

        int last = maximier.peek().value;
        while (!maximier.isEmpty()) {
            final int current = maximier.poll().value;
            assertTrue(current + " should be >= " + last, current >= last);
            last = current;
        }

    }

    private static class IdInteger implements Identified {

        final private int value;

        final private int id;

        public IdInteger(final int value) {
            this.value = value;
            this.id = BinomialHeapTest.id++;
        }

        @Override
        public int getId() {
            return id;
        }

        public String toString() {
            return Integer.toString(value);
        }
    }
}
