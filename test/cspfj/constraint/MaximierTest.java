package cspfj.constraint;

import java.util.Comparator;
import java.util.Random;

import org.junit.Test;

import cspfj.util.Heap;

public class MaximierTest {

	@Test
	public void test() {
		Heap<Integer> maximier = new Heap<Integer>(new IntegerComparator(),
				new Integer[100]);

		final Random random = new Random();

		for (int i = 0; i < 100; i++) {
			maximier.add(random.nextInt(1000));
		}

		System.out.println(maximier);

		while (!maximier.isEmpty()) {
			System.out.print(maximier.pull() + " ");
		}
		System.out.println();

	}

	private final static class IntegerComparator implements Comparator<Integer> {

		@Override
		public int compare(Integer o1, Integer o2) {
			return o1.compareTo(o2);
		}

	}
}
