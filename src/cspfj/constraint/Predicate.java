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

import java.util.logging.Logger;

import cspfj.exception.FailedGenerationException;
import gnu.jel.CompilationException;
import gnu.jel.CompiledExpression;
import gnu.jel.Evaluator;
import gnu.jel.Library;

public final class Predicate {

    private final StringBuffer parametersSB;

    private final StringBuffer expressionSB;

    private final static Logger logger = Logger
            .getLogger("cspfj.problem.Predicate");

    private final static Library lib;

    static {
        // Set up the library

        lib = new Library(new Class[] { PredicateOperations.class },
                new Class[] { VariableProvider.class }, null, null, null);

    }

    public Predicate() throws FailedGenerationException {
        super();

        parametersSB = new StringBuffer();
        expressionSB = new StringBuffer();
    }

    public void addPredicateParametersChar(final char[] ch, final int start,
            final int length) {
        parametersSB.append(ch, start, length);
    }

    public void addExpressionChar(final char[] ch, final int start,
            final int length) {
        expressionSB.append(ch, start, length);
    }

    public VariableProvider getNewVariableProvider() {
        return new VariableProvider(parametersSB.toString().trim());
    }

    public CompiledExpression compile(final VariableProvider variables)
            throws FailedGenerationException {

        final String expr = variables.getExpr(expressionSB.toString().trim());

        // Compile
        try {
            return Evaluator.compile(expr, lib);
        } catch (CompilationException ce) {
            logger.finer(expr);
            logger.throwing("Predicate", "compile", ce);
            throw new FailedGenerationException("COMPILATION ERROR :"
                    + ce.getMessage());
        }

    }

}
