package cspfj.problem;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

import cspfj.exception.FailedGenerationException;
import cspfj.util.BitVector;
import cspom.variable.CSPOMVariable;

public class VariableTest {

	private Problem problem;
	private Variable variable;

	@Before
	public void setup() throws FailedGenerationException {
		problem = new Problem();
		variable = problem
				.addVariable("V0", new BitVectorDomain(1, 2, 3, 4, 5));
		problem.prepareVariables();
		problem.prepareConstraints();
	}

	@Test
	public void testGetDomainSize() {
		assertEquals(5, variable.getDomainSize());
		problem.push();
		variable.remove(0);
		assertEquals(4, variable.getDomainSize());
		problem.pop();
		assertEquals(5, variable.getDomainSize());

	}

	@Test
	public void testIndex() {
		assertEquals(1, variable.getDomain().index(2));
	}

	@Test
	public void testIsAssigned() {
		assertFalse(variable.isAssigned());
		variable.assign(0, problem);
		assertTrue(variable.isAssigned());

	}

	@Test
	public void testIsPresent() {
		assertTrue(variable.isPresent(0));
		problem.push();
		variable.remove(0);
		assertFalse(variable.isPresent(0));
		problem.pop();
		assertTrue(variable.isPresent(0));
	}

	@Test
	public void testAssign() {
		assertEquals(1, problem.getNbFutureVariables());
		variable.assign(0, problem);
		assertEquals(0, problem.getNbFutureVariables());
		assertFalse(variable.isPresent(1));
		assertTrue(variable.isPresent(0));
	}

	@Test
	public void testUnassign() {
		variable.setLevel(1);
		variable.assign(0, problem);
		variable.unassign(problem);
		assertEquals(1, problem.getNbFutureVariables());
		variable.restoreLevel(0);
		assertTrue(variable.isPresent(1));
		assertTrue(variable.isPresent(0));
	}

	@Test
	public void testRestore() {
		variable.remove(1);
		variable.setLevel(1);
		variable.remove(2);
		variable.setLevel(3);
		variable.remove(3);
		assertFalse(variable.isPresent(1));

		variable.restoreLevel(0);
		assertTrue(variable.isPresent(3));
		assertTrue(variable.isPresent(2));
		assertFalse(variable.isPresent(1));

		variable.setLevel(3);
		variable.remove(3);
		variable.restoreLevel(0);
		assertTrue(variable.isPresent(3));
		assertTrue(variable.isPresent(2));
		assertFalse(variable.isPresent(1));
	}

	@Test
	public void testRestore2() throws FailedGenerationException {

		final Problem problem = new Problem();

		final Variable variable = problem.addVariable("V1",
				new BitVectorDomain(1, 2, 3, 4, 5, 6));
		problem.prepareVariables();

		variable.remove(0);
		variable.remove(1);
		assertArrayEquals(new int[] { 2, 3, 4, 5 }, variable
				.getCurrentIndexes());
		variable.setLevel(1);
		variable.assign(2, problem);
		variable.unassign(problem);
		variable.restoreLevel(0);
		assertArrayEquals(new int[] { 2, 3, 4, 5 }, variable
				.getCurrentIndexes());
	}

	@Test
	public void testGetFirst() {
		assertEquals(0, variable.getFirst());
		variable.remove(0);
		assertEquals(1, variable.getFirst());
	}

	@Test
	public void testGetLast() {
		assertEquals(4, variable.getLast());
		variable.remove(4);
		assertEquals(3, variable.getLast());
	}

	@Test
	public void testGetCurrentIndexes() {
		assertArrayEquals(new int[] { 0, 1, 2, 3, 4 }, variable
				.getCurrentIndexes());
		variable.remove(1);
		assertArrayEquals(new int[] { 0, 2, 3, 4 }, variable
				.getCurrentIndexes());
	}

	@Test
	public void testGetDomainAtLevel() {
		variable.remove(1);
		variable.setLevel(1);
		variable.remove(2);
		variable.setLevel(3);
		variable.remove(3);
		assertFalse(variable.isPresent(1));

		variable.restoreLevel(0);
		assertTrue(variable.isPresent(3));
		assertTrue(variable.isPresent(2));
		assertFalse(variable.isPresent(1));

		variable.setLevel(3);
		variable.remove(3);

		// long[] domain = variable.getDomainAtLevel(1);
		// long[] vector = BitVector.newBitVector(5, true);
		// BitVector.clear(vector, 1);
		// BitVector.clear(vector, 2);
		// assertArrayEquals(vector, domain);

	}

	@Test
	public void testGetNext() {
		assertEquals(2, variable.getNext(1));
		assertEquals(-1, variable.getNext(4));
		variable.remove(2);
		assertEquals(3, variable.getNext(1));
		assertEquals(3, variable.getNext(2));
		variable.remove(4);
		assertEquals(-1, variable.getNext(3));
	}

	@Test
	public void testGetPrev() {
		assertEquals(1, variable.getPrev(2));
		assertEquals(-1, variable.getPrev(0));
		variable.remove(2);
		assertEquals(1, variable.getPrev(3));
		assertEquals(1, variable.getPrev(2));
		variable.remove(0);
		assertEquals(-1, variable.getPrev(1));
	}

	@Test
	public void testGetLastAbsent() {
		assertEquals(-1, variable.getLastAbsent());
		variable.remove(2);
		assertEquals(2, variable.getLastAbsent());
		variable.remove(4);
		assertEquals(4, variable.getLastAbsent());
	}

	@Test
	public void testGetPrevAbsent() {
		assertEquals(-1, variable.getPrevAbsent(3));
		variable.remove(2);
		assertEquals(2, variable.getPrevAbsent(3));
		assertEquals(-1, variable.getPrevAbsent(2));
	}

	@Test
	public void testGetBitDomain() {
		assertEquals(BitVector.factory(5, true), variable.getBitDomain());
	}

	// @Test
	// public void testClone() throws CloneNotSupportedException {
	// final Variable clone = variable.clone();
	// assertNotSame(clone, variable);
	// assertNotSame(clone.getBitDomain(), variable.getBitDomain());
	// }

}
