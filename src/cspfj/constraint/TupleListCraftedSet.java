package cspfj.constraint;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class TupleListCraftedSet implements Matrix, Cloneable {

	private Map<Long, Collection<int[]>> list;

	private final boolean initialContent;

	public TupleListCraftedSet(final int[] sizes, final boolean initialContent) {
		this.initialContent = initialContent;
		list = new HashMap<Long, Collection<int[]>>();
	}

	private static long hash(final int[] tuple) {
		long hash = 3581;
		for (int i = tuple.length; --i >= 0;) {
			hash = (hash * 33) ^ tuple[i];
		}
		return hash;
	}

	@Override
	public boolean check(final int[] tuple) {
		return contains(tuple, list.get(hash(tuple))) ^ initialContent;
	}

	private static boolean contains(final int[] tuple,
			final Collection<int[]> array) {
		if (array == null) {
			return false;
		}
		for (int[] t : array) {
			if (Arrays.equals(t, tuple)) {
				return true;
			}
		}
		return false;
	}

	private static void remove(final int[] tuple, final Collection<int[]> array) {
		if (array == null) {
			return;
		}
		final Iterator<int[]> itr = array.iterator();

		while (itr.hasNext()) {
			if (Arrays.equals(itr.next(), tuple)) {
				itr.remove();
				break;
			}
		}
	}

	@Override
	public void set(final int[] tuple, final boolean status) {
		if (status == initialContent) {
			remove(tuple, list.get(hash(tuple)));
		} else {
			Collection<int[]> localList = list.get(hash(tuple));
			if (localList == null) {
				localList = new ArrayList<int[]>();
				list.put(hash(tuple), localList);
			}
			if (!contains(tuple, localList)) {
				localList.add(tuple.clone());
			}
		}
	}

	public TupleListCraftedSet clone() throws CloneNotSupportedException {
		final TupleListCraftedSet list = (TupleListCraftedSet) super.clone();

		// list.list = new HashSet<BigInteger>(this.list);

		return list;
	}
	//

	//
	// final static BigInteger sfactor = BigInteger.valueOf(33);
	//	
	// private static BigInteger hashBig(final int[] tuple) {
	// BigInteger hash = BigInteger.ONE;
	//
	// for (int i = tuple.length; --i >= 0;) {
	// hash = hash.multiply(sfactor).add(BigInteger.valueOf(tuple[i]));
	// }
	//
	// return hash;
	// }
	//
	// public static void main(String[] args) {
	// final int[] tuple = new int[Integer.valueOf(args[0])];
	// for (int i = tuple.length ; --i>=0;) {
	// tuple[i]=Integer.valueOf(args[1])+i;
	// }
	// final int tests = Integer.valueOf(args[2]);
	// long start = -CpuMonitor.getCpuTimeNano();
	// for (int i = tests ; --i>=0;) {
	// hashBig(tuple);
	// }
	// start += CpuMonitor.getCpuTimeNano();
	// System.out.println("Big : " + start/1e9d);
	//		
	// start = -CpuMonitor.getCpuTimeNano();
	// for (int i = tests ; --i>=0;) {
	// hashInt(tuple);
	// }
	// start += CpuMonitor.getCpuTimeNano();
	// System.out.println("long : " + start/1e9d);
	//		
	// }
	//	
}
