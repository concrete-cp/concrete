package cspfj.priorityqueues;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Queue;
import java.util.Random;

import org.junit.Test;

public class SoftHeapTest {

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
    public final void test() {
        final Queue<IdInteger> maximier = new SoftHeap<IdInteger>(
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

        int errors = 0;
        int last = maximier.peek().value;
        while (!maximier.isEmpty()) {
            final int current = maximier.poll().value;
            // System.out.println(current);
            // assertTrue(current + " should be >= " + last, current >= last);
            if (current < last) {
                errors++;
            }
            last = current;

        }
        System.out.println(errors + " errors (rate = " + (float) errors
                / INTS.length + ")");

    }

    private static class IdInteger implements Identified {

        private final int value;

        private final int id;

        public IdInteger(final int value) {
            this.value = value;
            this.id = SoftHeapTest.id++;
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
