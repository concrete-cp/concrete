package cspfj.constraint;

import java.math.BigInteger;
import java.util.HashSet;
import java.util.Set;

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

}
