package cspfj.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public final class Statistics {

	private Statistics() {
	}

	public static <T extends Comparable<T>> T median(final Collection<T> list) {
		final List<T> sorted = new ArrayList<T>(list);
		Collections.sort(sorted);
		return sorted.get(sorted.size() / 2);
	}

	public static double averageInt(final Collection<Integer> list) {
		int sum = 0;
		for (int i : list) {
			sum += i;
		}
		return (double) sum / list.size();
	}

	public static double stdevInt(final Collection<Integer> coll,
			final double avg) {
		double dev = 0;
		for (int i : coll) {
			dev += (i - avg) * (i - avg);
		}
		return Math.sqrt(dev / (coll.size() - 1));
	}

	public static double averageDbl(final Collection<Double> list) {
		double sum = 0;
		for (double i : list) {
			sum += i;
		}
		return sum / list.size();
	}

	public static double stdevDbl(final Collection<Double> coll,
			final double avg) {
		double dev = 0;
		for (double i : coll) {
			dev += (i - avg) * (i - avg);
		}
		return Math.sqrt(dev / (coll.size() - 1));
	}
}
