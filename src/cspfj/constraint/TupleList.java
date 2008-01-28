package cspfj.constraint;

import java.math.BigInteger;
import java.util.HashSet;
import java.util.Set;

public class TupleList implements Matrix, Cloneable {

	private Set<BigInteger> list;

	private final BigInteger[] factors;

	private final boolean supports;

	public TupleList(int[] sizes, boolean supports) {
		this.supports = supports ^true;
		list = new HashSet<BigInteger>();

		final int arity = sizes.length;
		factors = new BigInteger[arity];
		factors[0] = BigInteger.ONE;
		BigInteger maxDomain = BigInteger.ZERO;
		for (int i : sizes) {
			final BigInteger bi = BigInteger.valueOf(i);
			if (maxDomain.compareTo(bi) < 0) {
				maxDomain = bi;
			}
		}

		for (int i = 1; i < arity; i++) {
			factors[i] = factors[i - 1].multiply(maxDomain);
		}
	}

	private BigInteger hash(final int[] tuple) {
		BigInteger hash = BigInteger.ZERO;
		for (int i = tuple.length; --i >= 0;) {
			hash = hash.add(BigInteger.valueOf(tuple[i]).multiply(factors[i]));
		}

		return hash;
	}

	public boolean check(final int[] tuple) {
		return list.contains(hash(tuple)) ^ supports ^ true;
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status == supports) {
			list.add(hash(tuple));
		} else {
			list.remove(hash(tuple));
		}
	}

	public TupleList clone() throws CloneNotSupportedException {
		final TupleList list = (TupleList) super.clone();

		list.list = new HashSet<BigInteger>(this.list);

		return list;
	}

}
