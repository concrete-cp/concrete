package cspfj.priorityqueues;

import static org.junit.Assert.assertTrue;

import java.util.Random;

import org.junit.Test;

import cspfj.priorityqueues.BinaryHeap;
import cspfj.priorityqueues.Identified;
import cspfj.priorityqueues.Key;

public class BinaryHeapTest {

	private static int id = 0;

	private static final Random RANDOM = new Random();

	private static final IdInteger[] INTS;

	static {
		INTS = new IdInteger[1000000];
		for (int i = INTS.length; --i >= 0;) {
			INTS[i] = new IdInteger(RANDOM.nextInt(5000000));
		}
	}

	@Test
	public void test() {
		final BinaryHeap<IdInteger> maximier = new BinaryHeap<IdInteger>(
				new Key<IdInteger>() {

					@Override
					public int getKey(IdInteger object) {
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

		final private int value;

		final private int id;

		public IdInteger(final int value) {
			this.value = value;
			this.id = BinaryHeapTest.id++;
		}

		@Override
		public int getId() {
			return id;
		}

	}
}
