package cspfj;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;

import org.junit.Test;

public final class ParameterManagerTest {

    private static enum Enumeration {
        A, B, C;
    }

    @Test
    public void enumTest() {
        ParameterManager.registerEnum("enumTest", Enumeration.A);
        assertEquals(Enumeration.A, ParameterManager.getParameter("enumTest"));
        ParameterManager.parameter("enumTest", Enumeration.B);
        assertEquals(Enumeration.B, ParameterManager.getParameter("enumTest"));
        ParameterManager.parameterParse("enumTest", "C");
        assertEquals(Enumeration.C, ParameterManager.getParameter("enumTest"));
    }

    @Test
    public void pendingEnumTest() {
        ParameterManager.parameterParse("pendingEnumTest", "C");
        ParameterManager.registerEnum("pendingEnumTest", Enumeration.B);
        assertEquals(Enumeration.C,
                ParameterManager.getParameter("pendingEnumTest"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void emptyTest() {
        ParameterManager.getParameter("emptyTest");
    }

    @Test(expected = IllegalArgumentException.class)
    public void notRegisteredTest() {
        ParameterManager.parameterParse("notRegisteredTest", "C");
        ParameterManager.getParameter("notRegisteredTest");
    }

    @Test
    public void pendingTest() {
        ParameterManager.parameter("pendingTest", 1);
        ParameterManager.parameter("pendingTest", 2);
        ParameterManager.registerInteger("pendingTest", 3);
        assertEquals(2, ParameterManager.getParameter("pendingTest"));
    }

    @Test
    public void classTest() {
        ParameterManager.registerClass("classTest", Integer.class);
        assertEquals(Integer.class, ParameterManager.getParameter("classTest"));
        ParameterManager.parameter("classTest", Double.class);
        assertEquals(Double.class, ParameterManager.getParameter("classTest"));
        ParameterManager.parameterParse("classTest", "java.math.BigInteger");
        assertEquals(BigInteger.class,
                ParameterManager.getParameter("classTest"));
    }

}
