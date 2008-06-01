/**
 * CSPFJ - CSP solving API for Java
 * Copyright (C) 2006 Julien VION
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package cspfj.constraint;

import java.util.Collection;

import cspfj.exception.FailedGenerationException;
import cspfj.problem.Variable;

public class ExtensionConstraintGeneral extends AbstractExtensionConstraint {

//	private final static Logger logger = Logger
//			.getLogger(ExtensionConstraintGeneral.class.getSimpleName());

	public ExtensionConstraintGeneral(final Variable[] scope,
			final Matrix matrix) throws FailedGenerationException {
		super(scope, new MatrixManager(scope, matrix), true);
	}

	public ExtensionConstraintGeneral(final Variable[] scope,
			final Matrix matrix, final String name)
			throws FailedGenerationException {
		super(scope, new MatrixManager(scope, matrix), name, true);
	}

	public ExtensionConstraintGeneral deepCopy(
			final Collection<Variable> variables)
			throws CloneNotSupportedException {
		final ExtensionConstraintGeneral constraint = (ExtensionConstraintGeneral) super
				.deepCopy(variables);
		constraint.matrix = matrix.deepCopy(constraint.getScope(),
				constraint.tuple);
		return constraint;
	}

}
