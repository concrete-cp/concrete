package rb.randomlists;

/**
 * This class allows to perform some translations from base 10 to a given base.
 */
public class BasisConvertor {
	private static boolean controlNumber(int[] number, int base) {
		for (int i = 0; i < number.length; i++)
			if (number[i] >= base)
				return false;
		return true;
	}

	private static boolean controlNumber(int[] number, int[] bases) {
		for (int i = 0; i < number.length; i++)
			if (number[i] >= bases[i])
				return false;
		return true;
	}

	/**
	 * Returns the decimal value (value in base 10) that corresponds to the
	 * given number expressed in the given base. The given number is an array
	 * which contains digits in the given base.
	 * 
	 * @param number
	 *            the given number
	 * @param base
	 *            the given base
	 * @return the decimal value that corresponds to the given number in the
	 *         given base
	 */
	public static long getDecimalValueFor(int[] number, int base) {
		assert controlNumber(number, base);
		double decimalValue = number[0];
		for (int i = 1; i < number.length; i++)
			decimalValue = decimalValue * base + number[i];
		if (decimalValue > Long.MAX_VALUE)
			throw new IllegalArgumentException("The decimal value = "
					+ decimalValue
					+ "is too big to be represented by a long integer");
		return (long) decimalValue;
	}

	/**
	 * Returns the decimal value (value in base 10) that corresponds to the
	 * given number expressed using the given bases. The given number is an
	 * array which contains digits in the corresponding given bases.
	 * 
	 * @param number
	 *            the given number
	 * @param bases
	 *            the given bases
	 * @return the decimal value that corresponds to the given number expressed
	 *         using the given base
	 */
	public static long getDecimalValueFor(int[] number, int[] bases) {
		assert controlNumber(number, bases);
		double decimalValue = number[0];
		for (int i = 1; i < number.length; i++)
			decimalValue = decimalValue * bases[i] + number[i];
		if (decimalValue > Long.MAX_VALUE)
			throw new IllegalArgumentException("The decimal value = "
					+ decimalValue
					+ "is too big to be represented by a long integer");
		return (long) decimalValue;
	}

	/**
	 * Returns the value in the given base that corresponds to the given decimal
	 * value (value in base 10). This returned value corresponds to an array
	 * which contains digits in the given base.
	 * 
	 * @param decimalValue
	 *            the given decimal value
	 * @param value
	 *            the array used to store all the digits of the number in the
	 *            given base which corresponds to the given decimal value
	 * @param base
	 *            the given base
	 * @return the value in the given base
	 */
	public static int[] getValueFor(long decimalValue, int[] value, int base) {
		long decimal = decimalValue;
		for (int i = value.length - 1; i >= 0; i--) {
			value[i] = (int) (decimal % base);
			decimal = decimal / base;
		}
		if (decimal > 0) {
			throw new IllegalArgumentException(
					"The given array is too small to contain all the digits of the conversion");
		}
		return value;
	}

	/**
	 * Returns the value, expressed using the given bases, that corresponds to
	 * the given decimal value (value in base 10). This returned value
	 * corresponds to an array which contains digit, a digit for each base.
	 * 
	 * @param decimalValue
	 *            the given decimal value
	 * @param value
	 *            the array used to store all the digits of the number in the
	 *            given base which corresponds to the given decimal value
	 * @param bases
	 *            the given bases
	 * @return the value expressed using the given bases
	 */
	public static int[] getValueFor(long decimalValue, int[] value, int[] bases) {
		long decimal = decimalValue ;
		for (int i = value.length - 1; i >= 0; i--) {
			value[i] = (int) (decimal % bases[i]);
			decimal = decimal / bases[i];
		}
		if (decimal > 0)
			throw new IllegalArgumentException(
					"The given array is too small to contain all the digits of the conversion");
		return value;
	}

}
