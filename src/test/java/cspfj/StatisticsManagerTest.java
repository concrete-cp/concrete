package cspfj;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import cspfj.util.Statistic;

public final class StatisticsManagerTest {
    @SuppressWarnings("unused")
    private static class TestClass {
        @Statistic
        private static int testInt = 8;
        @Statistic
        private static long testLong = 8;
        @Statistic
        private static float testFloat = 9;
        @Statistic
        private static double testDouble = 9;

        @Statistic
        private int testDynInt;
        @Statistic
        private long testDynLong;
        @Statistic
        private float testDynFloat;
        @Statistic
        private double testDynDouble;

        public TestClass(int testDynInt, long testDynLong, float testDynFloat,
                double testDynDouble) {
            super();
            this.testDynInt = testDynInt;
            this.testDynLong = testDynLong;
            this.testDynFloat = testDynFloat;
            this.testDynDouble = testDynDouble;
        }
    }

    @Before
    public void setUp() throws Exception {
    }

    @Test
    public void testRegisterClassOfQ() {
        StatisticsManager.register(TestClass.class);

        assertEquals(8,
                StatisticsManager.get(TestClass.class.getName() + ".testInt"));
        assertEquals(9f,
                StatisticsManager.get(TestClass.class.getName() + ".testFloat"));
        assertEquals(8l,
                StatisticsManager.get(TestClass.class.getName() + ".testLong"));
        assertEquals(9d, StatisticsManager.get(TestClass.class.getName()
                + ".testDouble"));
    }

    @Test
    public void testRegisterStringObject() {
        StatisticsManager.register("test", new TestClass(8, 9, 10, 11));
        assertEquals(8, StatisticsManager.get("test.testDynInt"));
        assertEquals(9l, StatisticsManager.get("test.testDynLong"));
        assertEquals(10f, StatisticsManager.get("test.testDynFloat"));
        assertEquals(11d, StatisticsManager.get("test.testDynDouble"));
    }

    @Test
    public void testReset() {
        StatisticsManager.register(TestClass.class);
        StatisticsManager.register("test2", new TestClass(8, 9, 10, 11));
        StatisticsManager.reset();
        assertEquals(0,
                StatisticsManager.get(TestClass.class.getName() + ".testInt"));
        assertEquals(0l,
                StatisticsManager.get(TestClass.class.getName() + ".testLong"));
        assertEquals(0f,
                StatisticsManager.get(TestClass.class.getName() + ".testFloat"));
        assertEquals(0d, StatisticsManager.get(TestClass.class.getName()
                + ".testDouble"));
        assertEquals(4, StatisticsManager.digest().size());
    }

}
