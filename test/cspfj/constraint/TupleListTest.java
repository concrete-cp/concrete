/*
 * Created on 7 mai 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.constraint;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

public class TupleListTest {

	Matrix tls;

	Matrix tlc;

	@Before
	public void setUp() throws Exception {
		tls = new MatrixGeneral(new int[] { 6, 6, 6 }, false);
		tls = new TupleList(new int[] { 6, 6, 6 },false);
		tls.set(new int[] { 2, 2, 2 }, true);
		tls.set(new int[] { 2, 3, 3 }, true);
		tls.set(new int[] { 4, 2, 1 }, true);
		tls.set(new int[] { 2, 1, 1 }, true);

		tlc = new MatrixGeneral(new int[] { 6, 6, 6 }, true);
		tlc = new TupleList(new int[] { 6, 6, 6 },true);
		tlc.set(new int[] { 2, 2, 2 }, false);
		tlc.set(new int[] { 2, 3, 3 }, false);
		tlc.set(new int[] { 4, 2, 1 }, false);
		tlc.set(new int[] { 2, 1, 1 }, false);

	}

	@Test
	public void testCheck() {
		assertTrue(tls.check(new int[] { 2, 2, 2 }));
		assertTrue(tls.check(new int[] { 2, 3, 3 }));
		assertTrue(tls.check(new int[] { 4, 2, 1 }));
		assertTrue(tls.check(new int[] { 2, 1, 1 }));

		assertFalse(tls.check(new int[] { 1, 1, 1 }));
		assertFalse(tls.check(new int[] { 1, 4, 1 }));
		assertFalse(tls.check(new int[] { 2, 5, 1 }));
		assertFalse(tls.check(new int[] { 2, 4, 1 }));

		assertFalse(tlc.check(new int[] { 2, 2, 2 }));
		assertFalse(tlc.check(new int[] { 2, 3, 3 }));
		assertFalse(tlc.check(new int[] { 4, 2, 1 }));
		assertFalse(tlc.check(new int[] { 2, 1, 1 }));

		assertTrue(tlc.check(new int[] { 1, 1, 1 }));
		assertTrue(tlc.check(new int[] { 1, 4, 1 }));
		assertTrue(tlc.check(new int[] { 2, 5, 1 }));
		assertTrue(tlc.check(new int[] { 2, 4, 1 }));

	}

}
