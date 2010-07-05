package cspfj.priorityqueues;

import static org.junit.Assert.assertTrue;

import java.util.Random;

import org.junit.Test;

import cspfj.priorityqueues.FibonacciHeap;
import cspfj.priorityqueues.Identified;
import cspfj.priorityqueues.Key;

public final class FibonacciHeapTest {

    private static int id = 0;

    private static final Random RANDOM = new Random();
    private static final IdInteger[] INTS;

    static {
        INTS = new IdInteger[1000];
        for (int i = INTS.length; --i >= 0;) {
            INTS[i] = new IdInteger(RANDOM.nextInt(5000000));
        }
    }

    @Test
    public void test() {

        final FibonacciHeap<IdInteger> maximier = new FibonacciHeap<IdInteger>(
                new Key<IdInteger>() {

                    @Override
                    public float getKey(final IdInteger object) {
                        return object.value;
                    }

                });

        for (IdInteger i : INTS) {
            maximier.offer(i);
        }

        int last = maximier.peek().value;
        while (!maximier.isEmpty()) {
            final int current = maximier.poll().value;
            assertTrue(current >= last);
            last = current;
        }

    }

    private static class IdInteger implements Identified {

        private final int value;

        private final int id;

        public IdInteger(final int value) {
            this.value = value;
            this.id = FibonacciHeapTest.id++;
        }

        @Override
        public int getId() {
            return id;
        }
    }
}
