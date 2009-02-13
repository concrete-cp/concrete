package cspfj.problem;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.util.LargeBitVector;

public class VariableTest {

	Problem problem;
	Variable variable;

	@Before
	public void setup() throws FailedGenerationException {
		variable = new Variable(new int[] { 1, 2, 3, 4, 5 });
		problem = Problem.load(new ProblemGenerator() {
			@Override
			public void generate() throws FailedGenerationException {
				// TODO Auto-generated method stub

			}

			@Override
			public Collection<Constraint> getConstraints() {
				return new ArrayList<Constraint>();
			}

			@Override
			public List<Variable> getVariables() {
				return Arrays.asList(variable);
			}
		});

	}

	@Test
	public void testGetDomainSize() {
		assertEquals(5, variable.getDomainSize());
		variable.push();
		variable.remove(0, 1);
		assertEquals(4, variable.getDomainSize());
		variable.pop();
		assertEquals(5, variable.getDomainSize());

	}

	@Test
	public void testIndex() {
		assertEquals(1, variable.index(2));
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
		variable.pop();
		variable.remove(0, 0);
		assertFalse(variable.isPresent(0));
		variable.restore(0);
		assertTrue(variable.isPresent(0));
	}

	@Test
	public void testAssign() {
		assertEquals(1, problem.getNbFutureVariables());
		variable.assign(0, 0, problem);
		assertEquals(0, problem.getNbFutureVariables());
		assertFalse(variable.isPresent(1));
		assertTrue(variable.isPresent(0));
	}

	@Test
	public void testUnassign() {
		variable.assign(0, 0, problem);
		variable.unassign(problem);
		assertEquals(1, problem.getNbFutureVariables());
		variable.restore(0);
		assertTrue(variable.isPresent(1));
		assertTrue(variable.isPresent(0));
	}

	@Test
	public void testRestore() {
		variable.remove(1, 0);
		variable.remove(2, 1);
		variable.remove(3, 3);
		assertFalse(variable.isPresent(1));

		variable.restore(1);
		assertTrue(variable.isPresent(3));
		assertTrue(variable.isPresent(2));
		assertFalse(variable.isPresent(1));

		variable.remove(3, 3);
		variable.restore(0);
		assertTrue(variable.isPresent(3));
		assertTrue(variable.isPresent(2));
		assertTrue(variable.isPresent(1));
	}

	@Test
	public void testRestore2() throws FailedGenerationException {
		final Variable variable = new Variable(new int[] { 1, 2, 3, 4, 5, 6 });
		final Problem problem = Problem.load(new ProblemGenerator() {
			@Override
			public void generate() throws FailedGenerationException {
				// TODO Auto-generated method stub

			}

			@Override
			public Collection<Constraint> getConstraints() {
				return new ArrayList<Constraint>();
			}

			@Override
			public List<Variable> getVariables() {
				return Arrays.asList(variable);
			}
		});

		variable.remove(0, 1);
		variable.remove(1, 1);
		assertEquals(Arrays.asList(3, 4, 5, 6), variable.getCurrentDomain());
		variable.assign(2, 2, problem);
		variable.unassign(problem);
		variable.restore(2);
		assertEquals(Arrays.asList(3, 4, 5, 6), variable.getCurrentDomain());

		variable.remove(2, 1);
		variable.remove(4, 2);
		assertEquals(Arrays.asList(4, 6), variable.getCurrentDomain());

		variable.restore(2);
		assertEquals(Arrays.asList(4, 5, 6), variable.getCurrentDomain());

		variable.remove(5, 1);
		variable.restore(2);
		assertEquals(Arrays.asList(4, 6), variable.getCurrentDomain());
	}

	@Test
	public void testGetFirst() {
		assertEquals(0, variable.getFirst());
		variable.remove(0, 0);
		assertEquals(1, variable.getFirst());
	}

	@Test
	public void testGetLast() {
		assertEquals(4, variable.getLast());
		variable.remove(4, 0);
		assertEquals(3, variable.getLast());
	}

	@Test
	public void testEmpty() {
		variable.empty(0);
		assertFalse(variable.isPresent(0));
	}

	@Test
	public void testGetCurrentDomain() {
		assertEquals(Arrays.asList(1, 2, 3, 4, 5), variable.getCurrentDomain());
		variable.remove(1, 0);
		assertEquals(Arrays.asList(1, 3, 4, 5), variable.getCurrentDomain());
	}

	@Test
	public void testGetDomainAtLevel() {
		variable.remove(1, 0);
		variable.remove(2, 1);
		variable.remove(3, 3);
		assertFalse(variable.isPresent(1));

		variable.restore(1);
		assertTrue(variable.isPresent(3));
		assertTrue(variable.isPresent(2));
		assertFalse(variable.isPresent(1));

		variable.remove(3, 3);

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
		variable.remove(2, 0);
		assertEquals(3, variable.getNext(1));
		assertEquals(3, variable.getNext(2));
		variable.remove(4, 0);
		assertEquals(-1, variable.getNext(3));
	}

	@Test
	public void testGetPrev() {
		assertEquals(1, variable.getPrev(2));
		assertEquals(-1, variable.getPrev(0));
		variable.remove(2, 0);
		assertEquals(1, variable.getPrev(3));
		assertEquals(1, variable.getPrev(2));
		variable.remove(0, 0);
		assertEquals(-1, variable.getPrev(1));
	}

	@Test
	public void testGetLastAbsent() {
		assertEquals(-1, variable.getLastAbsent());
		variable.remove(2, 0);
		assertEquals(2, variable.getLastAbsent());
		variable.remove(4, 0);
		assertEquals(4, variable.getLastAbsent());
	}

	@Test
	public void testGetPrevAbsent() {
		assertEquals(-1, variable.getPrevAbsent(3));
		variable.remove(2, 0);
		assertEquals(2, variable.getPrevAbsent(3));
		assertEquals(-1, variable.getPrevAbsent(2));
	}

	@Test
	public void testGetBitDomain() {
		assertArrayEquals(LargeBitVector.newBitVector(5, true), variable
				.getBitDomain());
	}

	@Test
	public void testClone() throws CloneNotSupportedException {
		final Variable clone = variable.clone();
		assertNotSame(clone, variable);
		assertNotSame(clone.getBitDomain(), variable.getBitDomain());
	}

}
