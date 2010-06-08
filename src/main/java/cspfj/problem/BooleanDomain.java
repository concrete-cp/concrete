package cspfj.problem;

import java.util.Arrays;

import cspfj.util.BitVector;

public final class BooleanDomain implements Domain {
	private static final int HISTORY_INCREMENT = 20;

	private static final int[] UNKNOWN_ARRAY = new int[] { 0, 1 };
	private static final int[] TRUE_ARRAY = new int[] { 1 };
	private static final int[] FALSE_ARRAY = new int[] { 0 };
	private static final int[] EMPTY_ARRAY = new int[] {};

	private static final BitVector UNKNOWN_BV;
	private static final BitVector TRUE_BV;
	private static final BitVector FALSE_BV;
	private static final BitVector EMPTY_BV;

	static {
		UNKNOWN_BV = BitVector.factory(2, true);
		TRUE_BV = BitVector.factory(2, false);
		TRUE_BV.set(1);
		FALSE_BV = BitVector.factory(2, false);
		FALSE_BV.set(0);
		EMPTY_BV = BitVector.factory(2, false);
	}

	public static enum Status {
		UNKNOWN(2), TRUE(1), FALSE(1), EMPTY(0);

		private final int size;

		private Status(final int size) {
			this.size = size;
		}

		private int[] asArray() {
			switch (this) {
			case TRUE:
				return TRUE_ARRAY;
			case FALSE:
				return FALSE_ARRAY;
			case UNKNOWN:
				return UNKNOWN_ARRAY;
			case EMPTY:
				return EMPTY_ARRAY;
			default:
				throw new IllegalStateException();
			}
		}

		private BitVector asBitVector() {
			switch (this) {
			case TRUE:
				return TRUE_BV;
			case FALSE:
				return FALSE_BV;
			case UNKNOWN:
				return UNKNOWN_BV;
			case EMPTY:
				return EMPTY_BV;
			default:
				throw new IllegalStateException();
			}
		}

		@Override
		public String toString() {
			switch (this) {
			case UNKNOWN:
				return "[false, true]";
			case TRUE:
				return "[true]";
			case FALSE:
				return "[false]";
			case EMPTY:
				return "[]";
			default:
				throw new IllegalStateException();
			}
		}
	}

	private Status status;

	private Status[] history;

	private int currentLevel = 0;

	public BooleanDomain() {
		status = Status.UNKNOWN;
		history = new Status[HISTORY_INCREMENT];
	}

	public BooleanDomain(boolean constant) {
		if (constant) {
			status = Status.TRUE;
		} else {
			status = Status.FALSE;
		}
		history = new Status[HISTORY_INCREMENT];
	}

	@Override
	public int first() {
		switch (status) {
		case TRUE:
			return 1;
		case EMPTY:
			return -1;
		case FALSE:
		case UNKNOWN:
			return 0;
		default:
			throw new IllegalStateException();
		}
	}

	@Override
	public int size() {
		return status.size;
	}

	@Override
	public int last() {
		switch (status) {
		case FALSE:
			return 0;
		case EMPTY:
			return -1;
		case TRUE:
		case UNKNOWN:
			return 1;
		default:
			throw new IllegalStateException();
		}
	}

	@Override
	public int next(final int i) {
		if (i == 0 && status == Status.UNKNOWN) {
			return 1;
		}

		return -1;
	}

	@Override
	public int prev(final int i) {
		if (i == 1 && status == Status.UNKNOWN) {
			return 0;
		}

		return -1;
	}

	@Override
	public int lastAbsent() {
		throw new UnsupportedOperationException();
	}

	@Override
	public int prevAbsent(final int i) {
		throw new UnsupportedOperationException();
	}

	/**
	 * @param value
	 *            the value we seek the index for
	 * @return the index of the given value or -1 if it could not be found
	 */
	public int index(final int value) {
		return value;
	}

	/**
	 * @param index
	 *            index to test
	 * @return true iff index is present
	 */
	@Override
	public boolean present(final int index) {
		switch (status) {
		case UNKNOWN:
			return true;
		case TRUE:
			return index == 1;
		case FALSE:
			return index == 0;
		case EMPTY:
			return false;
		default:
			throw new IllegalStateException();
		}
	}

	@Override
	public void setSingle(final int index) {
		if (index == 0) {
			setStatus(Status.FALSE);
		} else {
			setStatus(Status.TRUE);
		}

	}

	public void setStatus(final Status status) {
		assert this.status == Status.UNKNOWN || this.status == status;

		this.status = status;
		if (history[currentLevel] == null) {
			history[currentLevel] = Status.UNKNOWN;
		}
	}

	@Override
	public int value(final int index) {
		return index;
	}

	@Override
	public void remove(final int index) {
		assert present(index);
		if (status.size == 1) {
			status = Status.EMPTY;
		} else if (index == 0) {
			status = Status.TRUE;
		} else {
			status = Status.FALSE;
		}

		if (history[currentLevel] == null) {
			history[currentLevel] = Status.UNKNOWN;
		}
	}

	public BitVector getBitVector() {
		return status.asBitVector();
	}

	public BooleanDomain clone() {
		final BooleanDomain clone;
		try {
			clone = (BooleanDomain) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new IllegalStateException(e);
		}
		clone.status = status;
		clone.history = history.clone();
		return clone;
	}

	@Override
	public int maxSize() {
		return 2;
	}

	@Override
	public void setLevel(final int level) {
		assert level > currentLevel;
		ensureCapacity(level);
		if (history[currentLevel] != null) {
			history[currentLevel] = status;
		}
		currentLevel = level;
	}

	private void ensureCapacity(final int size) {
		if (size >= history.length) {
			history = Arrays.copyOf(history, currentLevel + HISTORY_INCREMENT);
		}
	}

	@Override
	public void restoreLevel(final int level) {
		assert level < currentLevel;
		boolean change = false;
		for (int l = currentLevel; l > level; l--) {
			if (history[l] != null) {
				change = true;
				history[l] = null;
			}
		}
		if (change) {
			for (int l = level;; l--) {
				if (l < 0) {
					status = Status.UNKNOWN;
					break;
				}
				if (history[l] != null) {
					assert history[l] != Status.EMPTY;
					status = history[l];
					break;
				}
			}

		}
		currentLevel = level;
	}

	@Override
	public BitVector getAtLevel(final int level) {
		if (level < currentLevel) {
			for (int l = level; --l >= 0;) {
				if (history[l] != null) {
					return history[l].asBitVector();
				}
			}
			return Status.UNKNOWN.asBitVector();
		}

		return status.asBitVector();
	}

	@Override
	public int[] allValues() {
		return Status.UNKNOWN.asArray();
	}

	@Override
	public int[] currentValues() {
		return status.asArray();
	}

	@Override
	public String toString() {
		return status.toString();
	}

	public Status getStatus() {
		return status;
	}

	public boolean canBe(final boolean value) {
		switch (status) {
		case UNKNOWN:
			return true;
		case EMPTY:
			return false;
		case TRUE:
			return value;
		case FALSE:
			return !value;
		default:
			throw new IllegalStateException();
		}
	}

	@Override
	public int greatest(final int value) {
		throw new UnsupportedOperationException();
	}

	@Override
	public int removeFrom(final int lb) {
		switch (status) {
		case UNKNOWN:
			if (lb == 0) {
				status = Status.EMPTY;
				return 2;
			}
			if (lb == 1) {
				status = Status.FALSE;
				return 1;
			}
			return 0;
		case FALSE:
			if (lb == 0) {
				status = Status.EMPTY;
				return 1;
			}
			return 0;
		case TRUE:
			if (lb <= 1) {
				status = Status.EMPTY;
				return 1;
			}
			return 0;
		default:
			return 0;
		}

	}

	@Override
	public int removeTo(final int ub) {
		throw new UnsupportedOperationException();
	}

	@Override
	public int lowest(final int value) {
		switch (status) {
		case UNKNOWN:
		case FALSE:
			return value;
		case EMPTY:
			return -1;
		case TRUE:
			if (value == 0) {
				return -1;
			}
			return 1;
		default:
			throw new IllegalStateException();
		}
	}
}
