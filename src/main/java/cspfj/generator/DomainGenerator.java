package cspfj.generator;

import java.util.List;

import cspfj.problem.BitVectorDomain;
import cspfj.problem.IntDomain;
import cspom.variable.BooleanDomain;
import cspom.variable.CSPOMDomain;

public class DomainGenerator {
	public static IntDomain generate(final CSPOMDomain<?> cspomDomain) {
		if (cspomDomain == null) {
			return null;
		}
		if (cspomDomain instanceof BooleanDomain) {
			final BooleanDomain bD = (BooleanDomain) cspomDomain;
			if (bD.isConstant()) {
				if (bD.getBoolean()) {
					return new BitVectorDomain(1);
				}
				return new BitVectorDomain(0);
			}
			return new BitVectorDomain(0, 1);
		}
		return new BitVectorDomain((List<Integer>) cspomDomain.getValues());
	}
}
