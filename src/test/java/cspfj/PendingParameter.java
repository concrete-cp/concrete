package cspfj;

import cspfj.util.Parameter;

public class PendingParameter {
	@Parameter("pendingEnumTest")
	public static ParameterManagerTest.Enumeration pendingEnumTest = ParameterManagerTest.Enumeration.B;
	
	static {
		ParameterManager.register(PendingParameter.class);
	}
}
