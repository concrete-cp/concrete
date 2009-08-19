package cspfj.util;

import static org.junit.Assert.assertTrue;

import java.util.Random;

import org.junit.Test;

import cspfj.priorityqueues.BinaryHeap;
import cspfj.priorityqueues.Identified;
import cspfj.priorityqueues.Key;

public class HeapTest {

	private static int id = 0;

	@Test
	public void test() {
		final BinaryHeap<IdInteger> maximier = new BinaryHeap<IdInteger>(
				new Key<IdInteger>() {

					@Override
					public int getKey(IdInteger object) {
						return object.value;
					}

				}, 100);

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

	private static class IdInteger implements Identified {

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

	}
}
