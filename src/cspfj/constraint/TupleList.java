package cspfj.constraint;

import java.math.BigInteger;
import java.util.HashSet;
import java.util.Set;

import cspfj.util.CpuMonitor;

public class TupleList implements Matrix, Cloneable {

	private Set<BigInteger> list;

	private final boolean initialContent;

	private final BigInteger factor;

	public TupleList(final int[] sizes, final boolean initialContent) {
		this.initialContent = initialContent;
		list = new HashSet<BigInteger>();

		BigInteger maxDomain = BigInteger.ZERO;
		for (int i : sizes) {
			final BigInteger bi = BigInteger.valueOf(i);
			if (maxDomain.compareTo(bi) < 0) {
				maxDomain = bi;
			}
		}
		factor = maxDomain;

	}

	private BigInteger hash(final int[] tuple) {
		BigInteger hash = BigInteger.ONE;

		for (int i = tuple.length; --i >= 0;) {
			hash = hash.multiply(factor).add(BigInteger.valueOf(tuple[i]));
		}

		return hash;
	}

	@Override
	public boolean check(final int[] tuple) {
		return list.contains(hash(tuple)) ^ initialContent;
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status == initialContent) {
			list.remove(hash(tuple));
		} else {
			list.add(hash(tuple));
		}
	}

	public TupleList clone() throws CloneNotSupportedException {
		final TupleList list = (TupleList) super.clone();

		list.list = new HashSet<BigInteger>(this.list);

		return list;
	}
//
//	private static long hashInt(final int[] tuple) {
//		long hash = 3581;
//		for (int i = tuple.length; --i >= 0;) {
//			hash = (hash *33) + tuple[i];
//		}
//		return hash;
//	}
//
//	final static BigInteger sfactor = BigInteger.valueOf(33);
//	
//	private static BigInteger hashBig(final int[] tuple) {
//		BigInteger hash = BigInteger.ONE;
//
//		for (int i = tuple.length; --i >= 0;) {
//			hash = hash.multiply(sfactor).add(BigInteger.valueOf(tuple[i]));
//		}
//
//		return hash;
//	}
//
//	public static void main(String[] args) {
//		final int[] tuple = new int[Integer.valueOf(args[0])];
//		for (int i = tuple.length ; --i>=0;) {
//			tuple[i]=Integer.valueOf(args[1])+i;
//		}
//		final int tests = Integer.valueOf(args[2]);
//		long start = -CpuMonitor.getCpuTimeNano();
//		for (int i = tests ; --i>=0;) {
//			hashBig(tuple);
//		}
//		start += CpuMonitor.getCpuTimeNano();
//		System.out.println("Big : " + start/1e9d);
//		
//		start = -CpuMonitor.getCpuTimeNano();
//		for (int i = tests ; --i>=0;) {
//			hashInt(tuple);
//		}
//		start += CpuMonitor.getCpuTimeNano();
//		System.out.println("long : " + start/1e9d);
//		
//	}
//	
}
