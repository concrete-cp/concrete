package rb.randomlists;

import java.security.MessageDigest;

/**
 * This class allows managing implicit lists of random values. Using two fixed
 * prefixes, it is possible to associate a random byte value with any tuple of a
 * given fixed length. Different prefixes entail different behaviours. This
 * technique uses a hash function like SHA or MD5.
 */
public class ImplicitRandomList {
	/**
	 * The message digest algorithm.
	 */
	private MessageDigest md;

	/**
	 * Array used to store the message.
	 */
	private byte[] bytes;

	/**
	 * Builds a <code> ImplicitRandomList </code>
	 * 
	 * @param prefix1
	 *            the first prefix
	 * @param prefix2
	 *            the second prefix
	 * @param tupleLength
	 *            the length of the tuples
	 * @param algorithm
	 *            the name of the message digest algorithm
	 */
	public ImplicitRandomList(int prefix1, int prefix2, int tupleLength,
			String algorithm) {
		bytes = new byte[tupleLength + 3];
		// two bytes of the first prefix are fixed
		short s = (short) prefix1;
		bytes[tupleLength + 1] = (byte) (s & 0x00FF);
		bytes[tupleLength] = (byte) (s >> 8);
		// one byte of the second prefix is fixed
		s = (short) prefix2;
		// bytes[length+3]= (byte)(s & 0x00FF);
		bytes[tupleLength + 2] = (byte) (s & 0x00FF);
		try {
			md = MessageDigest.getInstance(algorithm);
		} catch (Exception e) {
			throw new IllegalArgumentException();
		}

	}

	/**
	 * Returns a random byte value from the given tuple.
	 * 
	 * @param tuple
	 *            agiven tuple
	 * @return a random byte value from the given tuple
	 */
	public final byte getRandomValueFor(int[] tuple) {
		for (int i = 0; i < tuple.length; i++)
			bytes[i] = (byte) (tuple[i] & 0x000000FF);

		byte[] digest = md.digest(bytes);
		byte resultat = digest[0];
		for (int i = 1; i < digest.length; i++)
			resultat = (byte) (resultat ^ digest[i]);
		return resultat; // + 128) / 255.0;
	}

	static void main(String[] args) {
		int[] t = { 123, 1, 12345, 0 };

		ImplicitRandomList ir = new ImplicitRandomList(10, 6, 4, "SHA");
		System.out.println("name " + ir.md.getAlgorithm() + "=> " + ir.md);

		for (int i = 0; i < 10000; i++) {
			byte d = ir.getRandomValueFor(t);
			if (i % 10000 == 0)
				System.out.println(i + "\t" + d);
			for (int j = 0; j < t.length; j++)
				t[j]++;
		}
	}
}
