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

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.problem.Variable;

public final class VariableProvider {
    private int[] tuple;

    private final Map<Integer, String> variables;

    private final static Logger logger = Logger
            .getLogger("cspfj.problem.VariableProvider");

    private final Map<Integer, Integer> positions;

    private int[] finalPositions;

    private final Map<Integer, Integer> constants;

    public VariableProvider(String parameters) {

        positions = new HashMap<Integer, Integer>();

        variables = new HashMap<Integer, String>();

        String[] paramList = parameters.split(" ");
        for (int i = 0; i < paramList.length; i += 2) {
            variables.put(i / 2, paramList[i + 1]);
        }

        constants = new HashMap<Integer, Integer>();

    }

    public void setTuple(final Variable[] variables, final int[] tuple) {
        for (int i = 0; i < tuple.length; i++) {
            this.tuple[i] = variables[i].getDomain()[tuple[i]];
        }
    }

    public String getExpr(final String expr) {

        String transformedExpr = expr;

        logger.finest(positions.toString());

        tuple = new int[positions.size()];

        // TODO : Attention aux noms de variable inclus !!
        for (int i = variables.size() - 1; i >= 0; i--) {
            if (constants.get(i) != null) {
                logger.finest("replacing " + variables.get(i) + " with "
                        + constants.get(i));
                transformedExpr = transformedExpr.replace(variables.get(i),
                        constants.get(i).toString());
            } else if (positions.get(i) != null) {
                logger.finest("replacing " + variables.get(i) + " with x(" + i
                        + ")");
                transformedExpr = transformedExpr.replace(variables.get(i),
                        "x(" + i + ")");
            } else {
                throw new IllegalArgumentException(Integer.toString(i));
            }
        }

        logger.finest(transformedExpr);

        int max = 0;
        for (int i : positions.keySet()) {
            if (i > max) {
                max = i;
            }
        }

        finalPositions = new int[max+1];
        for (int i : positions.keySet()) {
            finalPositions[i] = positions.get(i);
        }

        return transformedExpr;

    }

    public void setConstant(final int position, final int value) {
        constants.put(position, value);
    }

    public void setPosition(final int position, final int variablePosition) {
        positions.put(position, variablePosition);

    }

    public int x(final int i) {
        return tuple[finalPositions[i]];
    }
}