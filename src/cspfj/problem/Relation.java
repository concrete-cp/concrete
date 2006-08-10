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

package cspfj.problem;

import cspfj.exception.FailedGenerationException;

public final class Relation {

    private final int arity;

    private final int nbTuples;

    private final boolean supports;

    private final int version;

    private final StringBuffer stringBuffer;

    public Relation(final int arity, final int nbTuples,
            final String semantics, final int version) {
        super();
        this.arity = arity;
        this.nbTuples = nbTuples;
        this.supports = "supports".equals(semantics);
        this.version = version;

        stringBuffer = new StringBuffer();
    }

    public int getArity() {
        return arity;
    }

    public void addTuplesChar(final char[] ch, final int start, final int length) {
        stringBuffer.append(ch, start, length);
    }

    public boolean isSupports() {
        return supports;
    }

    public int[][] getTuples() throws FailedGenerationException {
        switch (version) {
        case 1:
            return parseV1(nbTuples, arity, stringBuffer.toString());

        case 2:
            return parseV2(nbTuples, arity, stringBuffer.toString());

        default:
            throw new FailedGenerationException("Unknown version");
        }

    }

    public int getNbTuples() {
        return nbTuples;
    }

    private static int[][] parseV1(final int nbTuples, final int arity,
            final String string) throws FailedGenerationException {
        final int[][] tuples = new int[nbTuples][arity];

        final String[] tupleList = string.substring(1).split("\\(");

        if (tupleList.length != nbTuples) {
            throw new FailedGenerationException("Inconsistent number of Tuples");
        }

        for (int i = 0; i < nbTuples; i++) {

            final String[] valueList = tupleList[i].substring(0,
                    tupleList[i].length() - 1).split(",");

            if (valueList.length != arity) {
                throw new FailedGenerationException("Incorrect arity");
            }

            for (int j = 0; j < arity; j++) {
                tuples[i][j] = Integer.parseInt(valueList[j]);
            }

        }

        return tuples;
    }

    private static int[][] parseV2(final int nbTuples, final int arity,
            final String string) throws FailedGenerationException {

        if (nbTuples < 1) {
            return new int[0][];
        }

        final int[][] tuples = new int[nbTuples][arity];

        final String[] tupleList = string.split("\\|");

        if (tupleList.length != nbTuples) {
            throw new FailedGenerationException(
                    "Inconsistent number of Tuples (" + tupleList.length
                            + " /= " + nbTuples + ") in " + string);
        }

        for (int i = 0; i < nbTuples; i++) {

            final String[] valueList = tupleList[i].trim().split(" ");

            if (valueList.length != arity) {
                throw new FailedGenerationException("Incorrect arity ("
                        + valueList.length + " /= " + arity + ") in "
                        + tupleList[i].trim());
            }

            for (int j = 0; j < arity; j++) {
                tuples[i][j] = Integer.parseInt(valueList[j]);
            }

        }

        return tuples;
    }

}
