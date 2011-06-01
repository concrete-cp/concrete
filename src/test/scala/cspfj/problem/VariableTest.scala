package cspfj.problem;

import cspfj.util.BitVector
import org.junit.Assert.{assertTrue, assertFalse, assertEquals, assertArrayEquals}
import org.junit.{Test, Before}

final class VariableTest {

  private var variable: Variable = null;

  @Before
  def setup() {
    variable = new Variable("V0", new BitVectorDomain(1, 2, 3, 4, 5));
  }

  @Test
  def testGetDomainSize() {
    assertEquals(5, variable.getDomainSize);
    variable.setLevel(1);
    variable.remove(0);
    assertEquals(4, variable.getDomainSize);
    variable.restoreLevel(0);
    assertEquals(5, variable.getDomainSize);

  }

  @Test
  def testIndex() {
    assertEquals(1, variable.getDomain().index(2));
  }

  @Test
  def testIsPresent() {
    assertTrue(variable.isPresent(0));
    variable.setLevel(1);
    variable.remove(0);
    assertFalse(variable.isPresent(0));
    variable.restoreLevel(0);
    assertTrue(variable.isPresent(0));
  }

  @Test
  def testAssign() {
    variable.setSingle(0);
    assertFalse(variable.isPresent(1));
    assertTrue(variable.isPresent(0));
  }

  @Test
  def testUnassign() {
    variable.setLevel(1);
    variable.setSingle(0);
    variable.restoreLevel(0);
    assertTrue(variable.isPresent(1));
    assertTrue(variable.isPresent(0));
  }

  @Test
  def testRestore() {
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
  def testRestore2() {

    val problem = new Problem();

    val variable = problem.addVariable("V1",
      new BitVectorDomain(1, 2, 3, 4, 5, 6));
    problem.prepare();

    variable.remove(0);
    variable.remove(1);
    assertArrayEquals(Array(2, 3, 4, 5),
      variable.getCurrentIndexes());
    variable.setLevel(1);
    variable.setSingle(2);
    variable.restoreLevel(0);
    assertArrayEquals(Array(2, 3, 4, 5),
      variable.getCurrentIndexes());
  }

  @Test
  def testGetFirst() {
    assertEquals(0, variable.getFirst());
    variable.remove(0);
    assertEquals(1, variable.getFirst());
  }

  @Test
  def testGetLast() {
    assertEquals(4, variable.getLast());
    variable.remove(4);
    assertEquals(3, variable.getLast());
  }

  @Test
  def testGetCurrentIndexes() {
    assertArrayEquals(Array(0, 1, 2, 3, 4),
      variable.getCurrentIndexes());
    variable.remove(1);
    assertArrayEquals(Array(0, 2, 3, 4),
      variable.getCurrentIndexes());
  }

  @Test
  def testGetDomainAtLevel() {
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
  def testGetNext() {
    assertEquals(2, variable.getNext(1));
    assertEquals(-1, variable.getNext(4));
    variable.remove(2);
    assertEquals(3, variable.getNext(1));
    assertEquals(3, variable.getNext(2));
    variable.remove(4);
    assertEquals(-1, variable.getNext(3));
  }

  @Test
  def testGetPrev() {
    assertEquals(1, variable.getPrev(2));
    assertEquals(-1, variable.getPrev(0));
    variable.remove(2);
    assertEquals(1, variable.getPrev(3));
    assertEquals(1, variable.getPrev(2));
    variable.remove(0);
    assertEquals(-1, variable.getPrev(1));
  }

  @Test
  def testGetLastAbsent() {
    assertEquals(-1, variable.getLastAbsent());
    variable.remove(2);
    assertEquals(2, variable.getLastAbsent());
    variable.remove(4);
    assertEquals(4, variable.getLastAbsent());
  }

  @Test
  def testGetPrevAbsent() {
    assertEquals(-1, variable.getPrevAbsent(3));
    variable.remove(2);
    assertEquals(2, variable.getPrevAbsent(3));
    assertEquals(-1, variable.getPrevAbsent(2));
  }

  @Test
  def testGetBitDomain() {
    assertEquals(BitVector.newBitVector(5, true), variable.getBitDomain());
  }

  // @Test
  // public void testClone() throws CloneNotSupportedException {
  // final Variable clone = variable.clone();
  // assertNotSame(clone, variable);
  // assertNotSame(clone.getBitDomain(), variable.getBitDomain());
  // }

}
