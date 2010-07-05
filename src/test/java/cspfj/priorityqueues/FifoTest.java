package cspfj.priorityqueues;

import static org.junit.Assert.assertEquals;

import java.util.Random;

import org.junit.Before;
import org.junit.Test;

public final class FifoTest {

    private static final int SIZE = 30;
    private static final Random RAND = new Random(0);
    private static int cId = 0;

    private static final class IdInteger implements Identified {
        private final int id;
        private int value;

        private IdInteger(final int value) {
            this.value = value;
            id = cId++;
        }

        public int getId() {
            return id;
        }

        public String toString() {
            return Integer.toString(value);
        }

    }

    @Before
    public void setUp() throws Exception {
    }

    @Test
    public void offerTest() {

        final Fifo<IdInteger> fifo = new Fifo<IdInteger>(new Key<IdInteger>() {

            @Override
            public float getKey(final IdInteger object) {
                return object.value;
            }
        });

        final IdInteger[] vals = new IdInteger[SIZE];
        for (int i = SIZE; --i >= 0;) {
            vals[i] = new IdInteger(RAND.nextInt(10000));
            fifo.offer(vals[i]);
        }

        for (int i = 20; --i >= 0;) {
            vals[i].value = RAND.nextInt(10);
            fifo.offer(vals[i]);
        }

        assertEquals(SIZE, fifo.size());

    }
}
