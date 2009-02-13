package cspfj.util;

import java.util.Comparator;
import java.util.Random;

public class RandomOrder<E> implements Comparator<E> {

	private final Random random;

	public RandomOrder(final Random random) {
		this.random = random;
	}

	public int compare(final E arg0, final E arg1) {
		return random.nextFloat() < .5 ? -1 : 1;
	}

}
