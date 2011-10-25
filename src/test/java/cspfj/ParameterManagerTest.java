package cspfj;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;

import org.junit.Test;

import cspfj.util.Parameter;

public final class ParameterManagerTest {

//	@Parameter("enumTest")
//	private static Enumeration enumTest = Enumeration.A;

	@Parameter("classTest")
	private static Class<?> classTest = Integer.class;
	
	@Parameter("intTest")
	private static int intTest = 12;

	static {
		ParameterManager.register(ParameterManagerTest.class);
	}

//	static enum Enumeration {
//		A, B, C;
//	}
//
//	@Test
//	public void enumTest() {
//		assertEquals(Enumeration.A, enumTest);
//		ParameterManager.parameter("enumTest", Enumeration.B);
//		assertEquals(Enumeration.B, enumTest);
//		ParameterManager.parameterParse("enumTest", "C");
//		assertEquals(Enumeration.C, enumTest);
//	}

//	@Test
//	public void pendingEnumTest() {
//		ParameterManager.parameterParse("pendingEnumTest", "C");
//		assertEquals(Enumeration.C, PendingParameter.pendingEnumTest);
//	}

	@Test
	public void classTest() {
		assertEquals(Integer.class, classTest);
		ParameterManager.parameter("classTest", Double.class);
		assertEquals(Double.class, classTest);
		ParameterManager.parameterParse("classTest", "java.math.BigInteger");
		assertEquals(BigInteger.class, classTest);
	}
	
	@Test
	public void intTest() {
	    assertEquals(12, intTest);
	    ParameterManager.parameter("intTest", 32);
	    assertEquals(32, intTest);
	}

}
