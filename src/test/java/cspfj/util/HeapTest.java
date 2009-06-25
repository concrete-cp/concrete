package cspfj.util;

import static org.junit.Assert.assertTrue;

import java.util.Comparator;
import java.util.Random;

import org.junit.Test;

import cspfj.util.Heap;

public class HeapTest {

    private static int id = 0;

    @Test
    public void test() {
        final Heap<IdInteger> maximier = new Heap<IdInteger>(
                new IdIntegerComparator(), new IdInteger[100]);

        final Random random = new Random();

        for (int i = 0; i < 100; i++) {
            maximier.add(new IdInteger(random.nextInt(1000)));
        }

        int last = maximier.peek().value;
        while (!maximier.isEmpty()) {
            final int current = maximier.poll().value;
            assertTrue(current >= last);
            last = current;
        }

    }

    private static class IdIntegerComparator implements
            Comparator<IdInteger> {

        @Override
        public int compare(IdInteger o1, IdInteger o2) {
            return o1.compareTo(o2);
        }

    }

    private static class IdInteger implements Identified, Comparable<IdInteger> {

        final private int value;

        final private int id;

        public IdInteger(final int value) {
            this.value = value;
            this.id = HeapTest.id++;
        }

        @Override
        public int getId() {
            return id;
        }

        @Override
        public int compareTo(IdInteger o) {
            return Integer.valueOf(value).compareTo(o.value);
        }

    }
}
