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

import gnu.jel.CompiledExpression;

import java.util.logging.Logger;

import cspfj.exception.FailedGenerationException;
import cspfj.problem.Variable;

public final class PredicateConstraint extends AbstractConstraint {
	private final Predicate predicate;

	private final StringBuffer parametersSB;

	private final VariableProvider variables;

	private final VariableProvider[] context;

	private CompiledExpression expr_c;

	private final static Logger logger = Logger
			.getLogger("cspfj.constraints.PredicateConstraint");

	public PredicateConstraint(final Variable[] scope, final Predicate predicate) {
		super(scope);

		this.predicate = predicate;

		variables = predicate.getNewVariableProvider();

		context = new VariableProvider[] { variables };

		parametersSB = new StringBuffer();
	}

	public void compileParameters() throws FailedGenerationException {
//		logger.finer("Compiling " + this.toString());

		final String[] parameters = parametersSB.toString().trim().split(" ");

		for (int i = 0; i < parameters.length; i++) {
			try {
				variables.setConstant(i, Integer.parseInt(parameters[i]));
			} catch (NumberFormatException e) {

				for (int v = 0; v < arity; v++) {
					if (parameters[i].equals(getInvolvedVariables()[v]
							.getName())) {
						variables.setPosition(i, v);
					}

				}

			}

		}

		expr_c = predicate.compile(variables);

	}

	public void addConstraintParametersChar(final char[] ch, final int start,
			final int length) {
		parametersSB.append(ch, start, length);
	}

	public boolean useTupleCache() {
		return true;
	}

	@Override
	public boolean check() {

		if (!super.check()) {
			return false ;
		}
		
		variables.setTuple(getInvolvedVariables(), tuple);

		boolean check = false ;
		
		try {
			check = expr_c.evaluate_boolean(context);
		} catch (Throwable e) {
			logger.throwing("PredicateConstraint", "check", e);

		}
		
		if (!check && matrix != null) {
			removeTuple(tuple) ;
		}
		return check;
	}

}
