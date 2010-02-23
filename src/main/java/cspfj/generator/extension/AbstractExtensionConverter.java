package cspfj.generator.extension;

import cspom.variable.Domain;

public abstract class AbstractExtensionConverter implements ExtensionConverter {


	protected final int[][] intDomains(final cspom.variable.CSPOMVariable[] signature) {
		final int[][] domains = new int[signature.length][];
		for (int i = signature.length; --i >= 0;) {
			domains[i] = this.domains.getIntDomain(signature[i].getDomain());

		}
		return domains;
	}

	protected final Domain[] cspomDomains(
			final cspom.variable.CSPOMVariable[] signature) {
		final Domain[] domains = new Domain[signature.length];
		for (int i = signature.length; --i >= 0;) {
			domains[i] = signature[i].getDomain();
		}
		return domains;
	}

	protected static int[] sizes(final Domain[] domains) {
		final int[] sizes = new int[domains.length];
		for (int i = sizes.length; --i >= 0;) {
			sizes[i] = domains[i].getNbValues();
		}
		return sizes;
	}

	protected static int[] sizes(final int[][] domains) {
		final int[] sizes = new int[domains.length];
		for (int i = sizes.length; --i >= 0;) {
			sizes[i] = domains[i].length;
		}
		return sizes;
	}

	public String toString() {
		return this.getClass().getSimpleName();
	}
}
