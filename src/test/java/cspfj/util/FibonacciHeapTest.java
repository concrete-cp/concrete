package cspfj.util;

import static org.junit.Assert.assertTrue;

import java.util.Random;

import org.junit.Test;

import cspfj.priorityqueues.FibonacciHeap;
import cspfj.priorityqueues.Identified;
import cspfj.priorityqueues.Key;

public class FibonacciHeapTest {

	private static int id = 0;

	private static final Random RANDOM = new Random();

	@Test
	public void test() {

		final IdInteger[] ints = new IdInteger[100];
		for (int i = 0; i < ints.length; i++) {
			ints[i] = new IdInteger(RANDOM.nextInt(1000));
		}

		final FibonacciHeap<IdInteger> maximier = new FibonacciHeap<IdInteger>(
				new Key<IdInteger>() {

					@Override
					public int getKey(IdInteger object) {
						return object.value;
					}

				}, 100);

		for (int i = 0; i < 100; i++) {
			maximier.add(ints[i]);
		}

		int last = maximier.peek().value;
		while (!maximier.isEmpty()) {
			final int current = maximier.poll().value;
			assertTrue(current >= last);
			last = current;
		}

	}

	private static class IdInteger implements Identified {

		final private int value;

		final private int id;

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