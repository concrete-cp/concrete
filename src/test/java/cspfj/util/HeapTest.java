package cspfj.util;

import static org.junit.Assert.assertTrue;

import java.util.Comparator;
import java.util.Random;

import org.junit.Test;

import cspfj.util.Heap;

public class HeapTest {

	@Test
	public void test() {
		final Heap<Integer> maximier = new Heap<Integer>(new IntegerComparator(),
				new Integer[100]);

		final Random random = new Random();

		for (int i = 0; i < 100; i++) {
			maximier.add(random.nextInt(1000));
		}

		int last = maximier.peek();
		while (!maximier.isEmpty()) {
			final int current = maximier.poll();
			assertTrue(current >= last);
			last = current;
		}

	}

	private final static class IntegerComparator implements Comparator<Integer> {

		@Override
		public int compare(Integer o1, Integer o2) {
			return o1.compareTo(o2);
		}

	}
}
